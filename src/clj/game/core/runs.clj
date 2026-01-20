(ns game.core.runs
  (:require
    [game.core.access :refer [breach-server]]
    [game.core.board :refer [get-zones server->zone]]
    [game.core.card :refer [get-card get-zone rezzed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [jack-out-cost run-cost run-additional-cost-bonus]]
    [game.core.effects :refer [any-effects get-effects]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid make-result]]
    [game.core.engine :refer [checkpoint end-of-phase-checkpoint register-pending-event pay queue-event resolve-ability trigger-event trigger-event-simult]]
    [game.core.flags :refer [can-run? clear-run-register!]]
    [game.core.gaining :refer [gain-credits]]
    [game.core.ice :refer [active-ice? break-subs-event-context get-current-ice get-run-ices update-ice-strength reset-all-ice reset-all-subs! set-current-ice]]
    [game.core.mark :refer [is-mark?]]
    [game.core.payment :refer [build-cost-string build-spend-msg can-pay? merge-costs ->c]]
    [game.core.prevention :refer [resolve-encounter-prevention resolve-end-run-prevention resolve-jack-out-prevention]]
    [game.core.prompts :refer [clear-run-prompts clear-wait-prompt show-run-prompts show-prompt show-wait-prompt]]
    [game.core.say :refer [play-sfx system-msg n-last-logs]]
    [game.core.servers :refer [is-remote? target-server unknown->kw zone->name]]
    [game.core.subtypes :refer [update-all-subtypes]]
    [game.core.to-string :refer [card-str]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability effect req wait-for]]
    [game.utils :refer [dissoc-in same-card?]]
    [jinteki.utils :refer [count-bad-pub other-side]]
    [clojure.stacktrace :refer [print-stack-trace]]
    [clojure.string :as string]
    [taoensso.timbre :as timbre]))

(declare handle-end-run jack-out forced-encounter-cleanup run-cleanup gain-run-credits pass-ice successful-run)

(defn total-run-cost
  ([state side card] (total-run-cost state side card nil))
  ([state side card {:keys [click-run ignore-costs] :as args}]
   (let [cost (let [cost (run-cost state side card nil args)]
                (when (and (pos? cost)
                           (not ignore-costs))
                  (->c :credit cost)))
         additional-costs (run-additional-cost-bonus state side card args)
         click-run-cost (when click-run (->c :click 1))]
     (when-not ignore-costs
       (merge-costs
         [click-run-cost
          cost
          additional-costs])))))

(defn- make-phase-eid
  [state eid]
  (or eid
      (make-eid state (:eid (:run @state)))))

(defn get-runnable-zones
  ([state] (get-runnable-zones state :runner (make-eid state) nil nil))
  ([state side] (get-runnable-zones state side (make-eid state) nil nil))
  ([state side card] (get-runnable-zones state side (make-eid state) card nil))
  ([state side card args] (get-runnable-zones state side (make-eid state) card args))
  ([state side eid card {:keys [zones ignore-costs]}]
   (let [restricted-zones (distinct (flatten (get-effects state side :cannot-run-on-server)))
         permitted-zones (remove (set restricted-zones) (or zones (get-zones state)))]
     (if ignore-costs
       permitted-zones
       (filter #(can-pay? state :runner eid card nil (total-run-cost state side card {:server (unknown->kw %)}))
               permitted-zones)))))

(defn can-run-server?
  [state server]
  (some #{(unknown->kw server)} (seq (get-runnable-zones state))))

(defn get-current-encounter
  [state]
  (peek (:encounters @state)))

(defn active-encounter?
  "Encounter is active when there is a current encounter and there is an active ice"
  [state]
  (and (get-current-encounter state)
       (active-ice? state)))

(defn update-current-encounter
  [state key value]
  (when-let [encounter (get-current-encounter state)]
    (let [updated-encounter (assoc encounter key value)]
      (swap! state update :encounters #(conj (pop %) updated-encounter)))))

(defn clear-encounter
  [state]
  (when-let [encounter (get-current-encounter state)]
    (swap! state update :encounters pop)
    (swap! state assoc :per-encounter nil)
    (effect-completed state nil (:eid encounter))))

(defn set-phase
  [state phase]
  (swap! state assoc-in [:run :phase] phase)
  (swap! state dissoc-in [:run :next-phase])
  (swap! state assoc-in [:run :no-action] false)
  phase)

(defn set-next-phase
  [state phase]
  (swap! state assoc-in [:run :next-phase] phase)
  phase)

(defmulti start-next-phase
  (fn [state _ _]
    (:next-phase (:run @state))))

(defmulti continue
  (fn [state _ _]
    (if (get-current-encounter state)
      :encounter-ice
      (:phase (:run @state)))))

(defn make-run
  "Starts a run on the given server, with the given card as the cause. If card is nil, assume a click was spent."
  ([state side eid server] (make-run state side eid server nil nil))
  ([state side eid server card] (make-run state side eid server card nil))
  ([state side eid server card {:keys [click-run ignore-costs] :as args}]
   (let [cost-args (assoc args :server (unknown->kw server))
         costs (total-run-cost state side card cost-args)
         card (or (get-card state card) card)
         eid (assoc eid :source-type :make-run)]
     (if-not (and (can-run? state :runner)
                  (can-run-server? state server)
                  (can-pay? state :runner eid card "a run" costs))
       (effect-completed state side eid)
       (do (swap! state dissoc-in [:end-run :ended])
           (when click-run
             (swap! state assoc-in [:runner :register :made-click-run] true)
             (play-sfx state side "click-run"))
           (wait-for
             (pay state :runner (make-eid state eid) nil costs)
             (let [payment-str (:msg async-result)]
               (if-not payment-str
                 (effect-completed state side eid)
                 (let [s [(if (keyword? server) server (last (server->zone state server)))]
                       ices (get-in @state (concat [:corp :servers] s [:ices]))
                       n (count ices)]
                   (when (not-empty payment-str)
                     (system-msg state :runner (str (build-spend-msg payment-str "make a run on" "makes a run on")
                                                    (zone->name (unknown->kw server))
                                                    (when ignore-costs ", ignoring all costs"))))
                   ;; s is a keyword for the server, like :hq or :remote1
                   (let [run-id (make-eid state)]
                     (swap! state assoc
                            :per-run nil
                            :run {:run-id run-id
                                  :server s
                                  :position n
                                  :corp-auto-no-action false
                                  :phase :initiation
                                  :eid eid
                                  :current-ice nil
                                  :events nil
                                  :source-card (select-keys card [:code :cid :zone :title :side :type :art :implementation])})
                     (when card
                       (update! state side (assoc-in card [:special :run-id] run-id))))
                   (show-run-prompts state (str "running on " (zone->name (unknown->kw server))) card)
                   (wait-for
                     (gain-run-credits state side
                                       (make-eid state eid)
                                       (+ (or (get-in @state [:runner :next-run-credit]) 0)
                                          (count-bad-pub state)))
                     (swap! state assoc-in [:runner :next-run-credit] 0)
                     (swap! state update-in [:runner :register :made-run] conj (first s))
                     (swap! state update-in [:stats side :runs :started] (fnil inc 0))
                     (queue-event state :run {:server s
                                              :position n
                                              :cost-args cost-args})
                     (end-of-phase-checkpoint state nil (make-eid state eid) :end-of-initiation)))))))))))

(defmethod continue :initiation
  [state side _]
  (if-not (get-in @state [:run :no-action])
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :corp side)
          (system-msg state side "has no further action")))
    (if (pos? (get-in @state [:run :position] 0))
      (do (set-next-phase state :approach-ice)
          (start-next-phase state side nil))
      (do (set-next-phase state :movement)
          (start-next-phase state side nil)))))

(defn toggle-auto-no-action
  [state _ _]
  (swap! state update-in [:run :corp-auto-no-action] not)
  (when (and (rezzed? (get-current-ice state))
             (#{:approach-ice :encounter-ice} (:phase (:run @state))))
    (continue state :corp nil)))

(defn check-auto-no-action
  "If corp-auto-no-action is enabled, presses continue for the corp as long as the only rezzed ice is approached or encountered."
  [state]
  (when (and (:run @state)
             (not (= :success (:phase (:run @state))))
             (not (and (= :movement (:phase (:run @state)))
                       (zero? (:position (:run @state)))))
             (<= (count (:encounters @state)) 1)
             (get-in @state [:run :corp-auto-no-action])
             (or (rezzed? (get-current-ice state))
                 (= :movement (:phase (:run @state)))))
    (continue state :corp nil)))

(defn check-for-empty-server
  [state]
  (let [run (:run @state)
        server (first (:server run))]
    (and run
         (is-remote? server)
         (empty? (get-in @state [:corp :servers server :content]))
         (empty? (get-in @state [:corp :servers server :ices])))))

(defn encounter-ends
  [state side eid]
  (let [encounter (get-current-encounter state)
        ice (get-current-ice state)]
    (update-current-encounter state :ending true)
    (when (:bypass encounter)
      (queue-event state :bypassed-ice ice)
      (system-msg state :runner (str "bypasses " (:title ice))))
    (wait-for (end-of-phase-checkpoint state nil (make-eid state eid)
                                       :end-of-encounter
                                       {:ice ice})
              (update-all-subtypes state)
              (let [run (:run @state)
                    phase (:phase run)]
                (cond
                  ;; end the run if server is empty
                  (check-for-empty-server state)
                  (do (clear-encounter state)
                      (handle-end-run state side eid))
                  ;; just clear the encounter if:
                  ;; * ETR effect has occurred
                  ;; * there are other encounters in progress
                  ;; * the encounter is outside of a run
                  ;; * the run is in the Success Phase
                  (or (:ended (:end-run @state))
                      (> (count (:encounters @state)) 1)
                      (not run)
                      (:successful run))
                  (do (reset-all-subs! state (get-card state ice))
                      (update-ice-strength state :corp (get-card state ice))
                      (clear-encounter state)
                      (effect-completed state side eid))
                  ;; change phase
                  (:next-phase (:run @state))
                  (do (clear-encounter state)
                      (start-next-phase state side eid))
                  ;; pass ice
                  (= :encounter-ice phase)
                  (do (clear-encounter state)
                      (set-next-phase state :movement)
                      (start-next-phase state side eid))
                  ;; unknown
                  :else (do (reset-all-subs! state (get-card state ice))
                            (update-ice-strength state :corp (get-card state ice))
                            (clear-encounter state)
                            (effect-completed state side eid)))))))

(defmethod start-next-phase :approach-ice
  [state side eid]
  (set-phase state :approach-ice)
  (set-current-ice state)
  (reset-all-ice state side)
  (swap! state assoc-in [:run :approached-ice?] true)
  (check-auto-no-action state)
  (let [eid (make-phase-eid state eid)
        ice (get-current-ice state)
        on-approach (:on-approach (card-def ice))]
    (system-msg state :runner (str "approaches " (card-str state ice)))
    (when on-approach
      (register-pending-event state :approach-ice ice on-approach))
    (queue-event state :approach-ice {:ice ice})
    (wait-for (checkpoint state nil
                          (make-eid state eid)
                          ;; Immediately end approach step if:
                          ;; * run ends
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (:ended (:end-run @state))
                                            (check-for-empty-server state)))})
              (if (or (check-for-empty-server state)
                      (:ended (:end-run @state)))
                (handle-end-run state side eid)
                (effect-completed state side eid)))))

(defmethod continue :approach-ice
  [state side _]
  (if-not (get-in @state [:run :no-action])
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :corp side)
          (system-msg state side "has no further action")))
    (let [eid (make-phase-eid state nil)
          approached-ice (get-card state (get-current-ice state))]
      (wait-for (end-of-phase-checkpoint state nil (make-eid state eid) :end-of-approach-ice)
                (cond
                  (or (check-for-empty-server state)
                      (:ended (:end-run @state)))
                  (handle-end-run state side eid)
                  (rezzed? approached-ice)
                  (do (set-next-phase state :encounter-ice)
                      (start-next-phase state :runner nil))
                  :else
                  (do (set-next-phase state :movement)
                      (start-next-phase state :runner nil)))))))

(defn bypass-ice
  [state]
  (update-current-encounter state :bypass true))

(defn can-bypass-ice
  [state side ice]
  (when-not (any-effects state side :bypass-ice false? ice)
    (:bypass (get-current-encounter state))))

(defn- should-end-encounter?
  "Immediately end encounter if:
    * run ends
    * ice is bypassed
    * ice has been moved
    * ice is installed but not rezzed
    * phase of run changes
    * server becomes empty"
  [state side ice]
  (or (:ended (:end-run @state))
      (can-bypass-ice state side (get-card state ice))
      (not (get-card state ice))
      (not (active-ice? state (get-card state ice)))
      (:next-phase (:run @state))
      (check-for-empty-server state)))

(defn- preventable-encounter-abi
  [abi ice]
  {:async true
   :interactive (req true)
   :ability-name (str (or (:ability-name abi) (:title ice)) " encounter")
   :effect (req (wait-for (resolve-encounter-prevention state side {:title (str (or (:ability-name abi) (:title ice)) " encounter") :card ice})
                          (if (pos? (:remaining async-result))
                            (do (register-pending-event state :resolve-ice-encounter-abi ice abi)
                                (queue-event state :resolve-ice-encounter-abi {:ice ice})
                                (checkpoint state side eid))
                            (effect-completed state side eid))))})

(defn encounter-ice
  ;; note: as far as I can tell, this deliberately leaves on open eid (the run eid).
  ;; Attempting to change that breaks a very large number of tests, so I'm leaving this
  ;; note here to remind me when I look at this later. -nbk, 2025
  ;;
  ;; TODO: rewrite forced encounter to use it's own version of encounter-ice,
  ;; then we can close the eids on this. Right now, closing the eids breaks
  ;; forced encounters and nothing else.
  [state side eid ice]
  (swap! state update :encounters conj {:eid eid
                                        :ice ice})
  (check-auto-no-action state)
  ;; step 6.9.3a: The encounter begins. Conditions relating to the Runner encountering
  ;; this ice are met (this is on-encounter effects, etc)
  (let [on-encounter (:on-encounter (card-def ice))
        applied-encounters (get-effects state nil :gain-encounter-ability ice)
        all-encounters (map #(preventable-encounter-abi % ice) (remove nil? (conj applied-encounters on-encounter)))]
    (system-msg state :runner (str "encounters " (card-str state ice {:visible (active-ice? state ice)})))
    (doseq [on-encounter all-encounters]
      (register-pending-event state :encounter-ice ice on-encounter))
    (queue-event state :encounter-ice {:ice ice})
    (wait-for (checkpoint state side
                          (make-eid state)
                          {:cancel-fn (fn [state]
                                        (should-end-encounter? state side ice))})
              (if (should-end-encounter? state side ice)
                (encounter-ends state side eid)
                ;; step 6.9.3b: if there are no subroutines on the ice,
                ;; the runner is considered to have broken all the subroutines on this ice.
                ;; This should fire an event, so it can get picked up with cards like hippo
                ;; or knifed.
                (when-let [c-ice (get-current-ice state)]
                  (when (and (same-card? c-ice ice) (zero? (count (:subroutines c-ice))))
                    (wait-for
                      (trigger-event-simult state side :subroutines-broken nil (break-subs-event-context state c-ice [] (get-in @state [:runner :basic-action-card])))
                      (when (should-end-encounter? state side ice)
                        (encounter-ends state side eid)))))))))

(defmethod start-next-phase :encounter-ice
  [state side _]
  (set-phase state :encounter-ice)
  (let [eid (make-phase-eid state nil)
        ice (get-current-ice state)]
    (encounter-ice state side eid ice)))

(defn force-ice-encounter
  ([state side eid ice] (force-ice-encounter state side eid ice nil))
  ([state side eid ice new-state]
   ;; clears the broken subs out of the prompt, otherwise they can get stuck with some cards
   (reset-all-subs! state (get-card state ice))
   (show-run-prompts state (str "encountering " (:title ice)) ice)
   ;; do we need to mess with the run state
   (let [old-state (get-in @state [:run :phase])]
     (when new-state
       (set-phase state new-state))
     (swap! state update :forced-encounter (fnil inc 0))
     (wait-for (encounter-ice state side (make-eid state eid) ice)
               (swap! state (fn [{:keys [forced-encounter] :as s}]
                              (if (and forced-encounter (> forced-encounter 1))
                                (update s :forced-encounter dec)
                                (dissoc s :forced-encounter))))
               ;; reset the state if needed
               (clear-run-prompts state)
               (if (and (not (:run @state))
                        (empty? (:encounters @state)))
                 (forced-encounter-cleanup state :runner eid)
                 (do
                   (when (and new-state (= new-state (get-in @state [:run :phase])))
                     (set-phase state old-state))
                   (set-current-ice state)
                   (effect-completed state side eid)))))))

(defmethod continue :encounter-ice
  [state side _]
  (let [encounter (get-current-encounter state)
        no-action (:no-action encounter)]
    (if (or (and no-action
                 (not= side no-action))
            (:bypass encounter))
      (encounter-ends state side (make-phase-eid state nil))
      (do (update-current-encounter state :no-action side)
          (when (= :runner side)
            (system-msg state side "has no further action"))))))

(defmethod start-next-phase :movement
  [state side eid]
  (let [eid (make-phase-eid state eid)
        previous-phase (:phase (:run @state))
        pos (get-in @state [:run :position])
        current-server (:server (:run @state))
        ice (get-current-ice state)
        ;; pass ice if coming from the Approach Ice or Encounter Ice phase and the Ice has not moved from the position
        pass-ice? (and (#{:approach-ice :encounter-ice} previous-phase)
                       (get-card state ice)
                       (= (second (get-zone ice)) (first current-server)))
        new-position (if pass-ice? (dec pos) pos)
        passed-all-ice (or (zero? new-position)
                           (= :initiation previous-phase))]
    (set-phase state :movement)
    (swap! state assoc-in [:run :no-action] false)
    (when pass-ice?
      (system-msg state :runner (str "passes " (card-str state ice)))
      (queue-event state :pass-ice {:ice (get-card state ice)}))
    (swap! state assoc-in [:run :position] new-position)
    (when passed-all-ice
      (queue-event state :pass-all-ice {:ice (get-card state ice)}))
    (check-auto-no-action state)
    (wait-for (checkpoint state side
                          (make-eid state)
                          ;; Immediately end pass ice step if:
                          ;; * run ends
                          ;; * run is moved to another server
                          ;; * phase changed
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (:ended (:end-run @state))
                                            (not= current-server (:server (:run @state)))
                                            (:next-phase (:run @state))
                                            (check-for-empty-server state)))})
              (reset-all-ice state side)
              (cond
                ;; run ended
                (or (check-for-empty-server state)
                    (:ended (:end-run @state)))
                (handle-end-run state side eid)
                ;; phase changed
                (:next-phase (:run @state))
                (start-next-phase state side eid)
                ;; stop in Movement phase
                :else (effect-completed state side eid)))))

(defn approach-server
  [state side eid]
  (set-current-ice state nil)
  (wait-for
    (trigger-event-simult state side :pre-approach-server nil
                          {:server (first (:server (:run @state)))})
    (system-msg state :runner (str "approaches " (zone->name (:server (:run @state)))))
    (queue-event state :approach-server)
    (wait-for (checkpoint state side
                          (make-eid state)
                          ;; Immediately end approach if:
                          ;; * run ends
                          ;; * phase changes
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (check-for-empty-server state)
                                            (:ended (:end-run @state))
                                            (get-in @state [:run :next-phase])))})
              (cond
                ;; end run
                (or (check-for-empty-server state)
                    (:ended (:end-run @state)))
                (handle-end-run state side eid)
                ;; phase changed
                (get-in @state [:run :next-phase])
                (start-next-phase state side eid)
                ;; go to Success phase
                :else (do (set-next-phase state :success)
                          (start-next-phase state side eid))))))

(defmethod continue :movement
  [state side _]
  (if-not (get-in @state [:run :no-action])
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :runner side)
          (system-msg state side "will continue the run")))
    (let [eid (make-phase-eid state nil)]
      (cond (or (check-for-empty-server state)
                (:ended (:end-run @state)))
            (handle-end-run state side eid)
            (pos? (get-in @state [:run :position]))
            (do (set-next-phase state :approach-ice)
                (start-next-phase state side eid))
            :else (approach-server state side eid)))))

(defmethod start-next-phase :success
  [state side _]
  (set-phase state :success)
  (if (check-for-empty-server state)
    (handle-end-run state side (make-phase-eid state nil))
    (successful-run state :runner)))

(defmethod start-next-phase :default
  [state _ _]
  (when-not (= :success (:phase (:run @state)))
    (timbre/error (Exception. "no phase found and not accessing cards")
                  (str "start-next-phase :default:\n" (n-last-logs state 5) "\n"))))

(defmethod continue :default
  [state _ _]
  (timbre/error (Exception.
                  (str "Continue clicked at the wrong time, run phase: " (:phase (:run @state))))
                (str "continue :default:\n" (n-last-logs state 5) "\n")))

(defn redirect-run
  ([state side server] (redirect-run state side server nil))
  ([state side server phase]
   (when (and (:run @state)
              (not= :success (:phase (:run @state))))
     (let [[_ dest] (server->zone state server)
           num-ice (count (get-in (:corp @state) [:servers dest :ices]))
           phase (if (= phase :approach-ice)
                   (if (pos? num-ice)
                     :approach-ice
                     :movement)
                   phase)]
       (trigger-event state side :pre-redirect-server
                      {:server (first (:server (:run @state)))
                       :new-server dest})
       (play-sfx state side "redirect")
       (swap! state update :run
              assoc
              :position num-ice
              :server [dest])
       (when phase
         (set-next-phase state phase)))
     (set-current-ice state))))

;; Non timing stuff
(defn gain-run-credits
  "Add temporary credits that will disappear when the run is over."
  [state _ eid n]
  (swap! state update-in [:runner :run-credit] (fnil + 0 0) n)
  (gain-credits state :runner eid n))

(defn gain-next-run-credits
  "Add temporary credits for the next run to be initiated."
  [state _ n]
  (swap! state update-in [:runner :next-run-credit] (fnil + 0 0) n))

;;; Ending runs
(defn add-run-effect
  [state card ability props]
  (let [ability {:card card
                 :mandatory (:mandatory props)
                 :ability ability}]
    (swap! state update-in [:run :run-effects] conj ability)))

(defn successful-run-replace-breach
  [props]
  (let [ability (:ability props)
        attacked-server (:target-server props)
        use-this-card-run (:this-card-run props)
        duration (:duration props)]
    {:event :successful-run
     :duration duration
     :req (req (and (if use-this-card-run this-card-run true)
                    (case attacked-server
                      (:archives :rd :hq)
                      (= attacked-server (target-server context))
                      :remote
                      (is-remote? (target-server context))
                      ; else
                      true)))
     :silent (req true)
     :effect (req (add-run-effect state card ability props))}))

(defn choose-replacement-ability
  [state handlers]
  (let [mandatory (some :mandatory handlers)
        titles (into [] (keep #(:card %) handlers))
        eid (make-phase-eid state nil)]
    (cond
      ;; If you can't access, there's nothing to replace
      (:prevent-access (:run @state))
      (handle-end-run state :runner eid)
      ;; Otherwise, if there's no handlers, access the cards
      (zero? (count titles))
      (wait-for (breach-server state :runner (make-eid state) (get-in @state [:run :server]))
                (handle-end-run state :runner eid))
      ;; If there's only 1 handler and it's mandatory, just execute it
      (and mandatory (= 1 (count titles)))
      (let [chosen (first handlers)
            ability (:ability chosen)
            card (:card chosen)]
        (system-msg state :runner (str "uses the replacement effect from " (:title card)))
        (wait-for (resolve-ability state :runner ability card [(select-keys (:run @state) [:server :run-id])])
                  (handle-end-run state :runner eid)))
      ;; there are multiple handlers or the handler is optional
      (pos? (count titles))
      (resolve-ability
        state :runner
        {:prompt "Choose a breach replacement ability"
         :choices (if mandatory titles (conj titles (str "Breach " (zone->name (:server (:run @state))))))
         :async true
         :effect (req (let [chosen (some #(when (same-card? target (:card %)) %) handlers)
                            ability (:ability chosen)
                            card (:card chosen)]
                        (if chosen
                          (do (system-msg state :runner (str "uses the replacement effect from " (:title card)))
                              (wait-for (resolve-ability state :runner ability card [(select-keys (:run @state) [:server :run-id])])
                                        (handle-end-run state :runner eid)))
                          (do (system-msg state :runner (str "chooses to breach " (zone->name (:server (:run @state))) " instead of use a replacement effect"))
                              (wait-for (breach-server state :runner (make-eid state eid) (get-in @state [:run :server]))
                                        (handle-end-run state :runner eid))))))}
        nil nil)
      ;; Just in case
      :else
      (run-cleanup state :runner eid))))

(defn prevent-access
  "Prevents the runner from accessing cards or breaching the server this run.
   This will cancel any run effects and not trigger breach/access routines."
  [state _]
  (swap! state assoc-in [:run :prevent-access] true))

(defn complete-run
  "This does all of the breach related stuff"
  [state side]
  (let [eid (make-phase-eid state nil)]
    (if (:ended (:end-run @state))
      (run-cleanup state :runner eid)
      (let [the-run (:run @state)
            server (:server the-run)
            replacement-effects (:run-effects the-run)]
        (cond
          ;; Prevented from breaching
          (:prevent-access the-run)
          (resolve-ability
            state :runner eid
            {:prompt (str "You are prevented from breaching " (zone->name server) " this run.")
             :choices ["OK"]
             :async true
             :effect (effect (system-msg :runner (str "is prevented from breaching " (zone->name server) " this run."))
                             (handle-end-run eid))}
            nil nil)

          ;; Any number of replace-breach effects
          (pos? (count replacement-effects))
          (choose-replacement-ability state replacement-effects)

          ;; No replace-breach effects
          :else
          (wait-for (breach-server state side (make-eid state eid) server)
                    (handle-end-run state side eid)))))))

(defn- register-successful-run
  [state side eid server]
  ;; TODO: :pre-successful-run exists merely for Omar Keung and Sneakdoor Beta
  ;; Needs prevention system to remove
  (queue-event state :pre-successful-run (select-keys (:run @state) [:server :run-id]))
  (wait-for (checkpoint state nil (make-eid state eid))
            (if (any-effects state side :block-successful-run)
              (effect-completed state side eid)
              (do (swap! state update-in [:runner :register :successful-run] conj (-> @state :run :server first))
                  (swap! state assoc-in [:run :successful] true)
                  ;; if the server is a mark, add it to the successful run
                  (let [marked (when (is-mark? state (first (:server (:run @state))))
                                 {:marked-server true})
                        keys (conj (select-keys (:run @state) [:server :run-id :subroutines-fired]) marked)]
                    (queue-event state :successful-run keys)
                    (checkpoint state nil eid))))))

(defn successful-run
  "The real 'successful run' trigger."
  [state side]
  (if (any-effects state side :block-successful-run)
    (complete-run state side)
    (wait-for (register-successful-run state side (make-phase-eid state nil) (get-in @state [:run :server]))
              (complete-run state side))))

(defn- register-unsuccessful-run
  [state side eid]
  (let [run (:run @state)]
    (swap! state update-in [:runner :register :unsuccessful-run] conj (first (:server run)))
    (swap! state assoc-in [:run :unsuccessful] true)
    (wait-for (handle-end-run state side (make-eid state eid))
              (queue-event state :unsuccessful-run run)
              (checkpoint state nil eid))))

(defn- resolve-end-run
  "End this run, and set it as UNSUCCESSFUL"
  ([state side eid]
   (if (or (not (:run @state))
           (get-in @state [:run :successful]))
     (handle-end-run state side eid)
     (register-unsuccessful-run state side eid))))

(defn end-run
  "After checking for prevents, end this run, and set it as UNSUCCESSFUL."
  ([state side eid card] (end-run state side eid card nil))
  ([state side eid card {:keys [unpreventable] :as args}]
   (if (or (:run @state)
           (get-current-encounter state))
     (wait-for (resolve-end-run-prevention state side (assoc args :card card))
               (if (pos? (:remaining async-result))
                 (resolve-end-run state side eid)
                 (effect-completed state side eid)))
     (effect-completed state side eid))))

(defn- resolve-jack-out
  [state side eid]
  (queue-event state :jack-out nil)
  (system-msg state side "jacks out")
  (end-run state side eid nil {:unpreventable true}))

(defn jack-out
  "The runner decides to jack out."
  ([state side eid]
   (if (any-effects state side :cannot-jack-out true?)
     (do (system-msg state :runner "cannot jack out this run")
         (complete-with-result state side eid false))
     (let [cost (jack-out-cost state side)]
       (if (can-pay? state side eid nil "jack out" cost)
         (wait-for (pay state :runner nil cost)
                   (if-let [payment-str (:msg async-result)]
                     (do (when-not (string/blank? payment-str)
                           (system-msg state :runner (str payment-str " to jack out")))
                         (wait-for (resolve-jack-out-prevention state side nil)
                                   (if (pos? (:remaining async-result))
                                     (resolve-jack-out state side eid)
                                     (complete-with-result state side eid false))))
                     (complete-with-result state side eid false)))
         (do (system-msg state :runner (str "attempts to jack out but can't pay (" (build-cost-string cost) ")"))
             (complete-with-result state side eid false)))))))

(defn- run-end-fx
  [state side {:keys [eid successful unsuccessful]}]
  (cond
    ;; Successful
    successful
    (do
      (play-sfx state side "run-successful")
      (complete-with-result state side eid {:successful true}))
    ;; Unsuccessful
    unsuccessful
    (do
      (play-sfx state side "run-unsuccessful")
      (complete-with-result state side eid {:unsuccessful true}))
    ;; Neither
    :else
    (complete-with-result state side eid nil)))

(defn run-cleanup
  "Trigger appropriate events for the ending of a run."
  [state side eid]
  (swap! state assoc-in [:end-run :ended] true)
  (when (get-current-encounter state)
    (queue-event state :end-of-encounter {:ice (get-current-ice state)}))
  (let [marked? (is-mark? state (get-in @state [:run :server 0]))
        run (if marked? (assoc (:run @state) :marked-server true) (:run @state))
        run-eid (:eid run)]
    (swap! state assoc-in [:runner :register :last-run] run)
    (swap! state update-in [:runner :credit] - (get-in @state [:runner :run-credit]))
    (swap! state assoc-in [:runner :run-credit] 0)
    (swap! state assoc :run nil)
    (swap! state dissoc-in [:end-run :ended])
    (queue-event state :run-ends run)
    (clear-encounter state)
    (clear-run-prompts state)
    (reset-all-ice state side)
    (clear-run-register! state)
    (wait-for (checkpoint state nil (make-eid state eid) {:durations [:end-of-encounter :end-of-run :end-of-next-run]})
              (run-end-fx state side run)
              (effect-completed state side eid)
              (effect-completed state side run-eid))))

(defn forced-encounter-cleanup
  "Trigger appropriate events for the end of an encounter outside of a run"
  [state side eid]
  (swap! state dissoc-in [:end-run :ended])
  (wait-for (checkpoint state nil (make-eid state eid) {:durations [:end-of-encounter :end-of-run]})
            (reset-all-ice state side)
            (swap! state assoc :per-encounter nil)
            (clear-run-register! state)
            (effect-completed state side eid)))

(defn handle-end-run
  "Initiate run resolution."
  [state side eid]
  (let [runner-prompt (first (:prompt (:runner @state)))
        corp-prompt (first (:prompt (:corp @state)))]
    (cond (and (:run @state)
               (not (get-current-encounter state))
               (or (nil? runner-prompt)
                   (= :run (:prompt-type runner-prompt)))
               (or (nil? corp-prompt)
                   (= :run (:prompt-type corp-prompt))))
          (run-cleanup state side eid)
          (and (not (:ended (:end-run @state)))
               (or (:run @state)
                   (get-current-encounter state)))
          (do (swap! state assoc-in [:end-run :ended] true)
              (when (:run @state)
                (prevent-access state side))
              (if (and (get-current-encounter state)
                       (not (:ending (get-current-encounter state))))
                (encounter-ends state side eid)
                (effect-completed state side eid)))
          :else (effect-completed state side eid))))

(defn total-cards-accessed
  ([run]
   (apply + (vals (:cards-accessed run {}))))
  ([run server]
   (get-in run [:cards-accessed server] 0)))
