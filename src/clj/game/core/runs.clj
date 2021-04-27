(ns game.core.runs
  (:require
    [game.core.access :refer [do-access]]
    [game.core.board :refer [server->zone]]
    [game.core.card :refer [get-card rezzed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [jack-out-cost run-cost run-additional-cost-bonus]]
    [game.core.effects :refer [any-effects unregister-floating-effects]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid make-result]]
    [game.core.engine :refer [checkpoint end-of-phase-checkpoint make-pending-event merge-costs-paid pay queue-event resolve-ability unregister-floating-events]]
    [game.core.flags :refer [can-run? can-run-server? cards-can-prevent? clear-run-register! get-prevent-list prevent-jack-out]]
    [game.core.gaining :refer [gain-credits]]
    [game.core.ice :refer [get-current-ice get-run-ices reset-all-ice set-current-ice]]
    [game.core.payment :refer [build-cost-string build-spend-msg can-pay? merge-costs]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [play-sfx system-msg]]
    [game.core.servers :refer [is-remote? target-server unknown->kw zone->name]]
    [game.core.to-string :refer [card-str]]
    [game.core.update :refer [update!]]
    [game.macros :refer [effect req wait-for]]
    [game.utils :refer [dissoc-in remove-once same-card?]]
    [jinteki.utils :refer [count-bad-pub]]
    [clojure.stacktrace :refer [print-stack-trace]]
    [clojure.string :as string]))

(declare handle-end-run jack-out run-cleanup gain-run-credits pass-ice successful-run)

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
    (:phase (:run @state))))

(defn complete-make-run
  [state side eid server card payment-str {:keys [click-run ignore-costs] :as args}]
  (let [s [(if (keyword? server) server (last (server->zone state server)))]
        ices (get-in @state (concat [:corp :servers] s [:ices]))
        n (count ices)]
    (when click-run
      (play-sfx state side "click-run")
      (system-msg state :runner (str (build-spend-msg payment-str "make a run on" "makes a run on")
                                     (zone->name (unknown->kw server))
                                     (when ignore-costs ", ignoring all costs"))))
    (let [run-id (make-eid state)]
      (swap! state assoc
             :per-run nil
             :run {:run-id run-id
                   :server s
                   :position n
                   :corp-auto-no-action false
                   :jack-out false
                   :jack-out-after-pass false
                   :phase :initiation
                   :next-phase :initiation
                   :eid eid
                   :current-ice nil
                   :events nil
                   :can-access true
                   :source-card (select-keys card [:code :cid :zone :title :side :type :art :implementation])})
      (when card
        (update! state side (assoc-in card [:special :run-id] run-id))))
    (wait-for (gain-run-credits state side
                                (make-eid state eid)
                                (+ (or (get-in @state [:runner :next-run-credit]) 0)
                                   (count-bad-pub state)))
              (swap! state assoc-in [:runner :next-run-credit] 0)
              (swap! state update-in [:runner :register :made-run] conj (first s))
              (swap! state update-in [:stats side :runs :started] (fnil inc 0))
              (queue-event state :run {:server s
                                       :position n
                                       :cost-args args})
              (wait-for
                (checkpoint state nil (make-eid state eid))
                (wait-for
                  (end-of-phase-checkpoint state nil (make-eid state eid) :end-of-initiation)
                  (if (pos? (get-in @state [:run :position] 0))
                    (do (set-next-phase state :approach-ice)
                        (start-next-phase state side nil))
                    (do (set-next-phase state :approach-server)
                        (swap! state assoc-in [:run :jack-out] true)
                        (start-next-phase state side nil))))))))

(defn total-run-cost
  ([state side card] (total-run-cost state side card nil))
  ([state side card {:keys [click-run ignore-costs cached-costs] :as args}]
   (or cached-costs
       (let [cost (let [cost (run-cost state side card nil args)]
                    (when (and (pos? cost)
                               (not ignore-costs))
                      [:credit cost]))
             additional-costs (run-additional-cost-bonus state side card args)
             click-run-cost (when click-run [:click 1])]
         (when-not ignore-costs
           (merge-costs
             [click-run-cost
              cost
              additional-costs]))))))

(defn can-make-run?
  ([state side eid server] (can-make-run? state side eid server nil nil))
  ([state side eid server card] (can-make-run? state side eid server card nil))
  ([state side eid server card args]
   (let [costs (total-run-cost state side card args)]
     (and (can-run? state :runner)
          (can-run-server? state server)
          (can-pay? state :runner eid card "a run" costs)))))

(defn make-run
  "Starts a run on the given server, with the given card as the cause. If card is nil, assume a click was spent."
  ([state side eid server] (make-run state side eid server nil nil))
  ([state side eid server card] (make-run state side eid server card nil))
  ([state side eid server card args]
   (let [args (assoc args :server (unknown->kw server))
         costs (total-run-cost state side card args)]
     (if (can-make-run? state side eid server card (assoc args :cached-costs costs))
       (wait-for (pay state :runner (make-eid state {:source card :source-type :make-run}) nil costs)
                 (let [payment-str (:msg async-result)
                       cost-paid (merge-costs-paid (:cost-paid eid) (:cost-paid async-result))]
                   (if payment-str
                     (complete-make-run state side (assoc eid :cost-paid cost-paid) server card payment-str args)
                     ;; payment failed????
                     (effect-completed state side eid))))
       ;; can't make a run on this server or can't pay to make a run
       (effect-completed state side eid)))))

(defn toggle-auto-no-action
  [state _ _]
  (swap! state update-in [:run :corp-auto-no-action] not)
  (when (and (rezzed? (get-current-ice state))
             (or (= :approach-ice (get-in @state [:run :phase]))
                 (= :encounter-ice (get-in @state [:run :phase]))))
    (continue state :corp nil)))

(defn check-auto-no-action
  "If corp-auto-no-action is enabled, presses continue for the corp as long as the only rezzed ice is approached or encountered."
  [state]
  (when (and (get-in @state [:run :corp-auto-no-action])
             (rezzed? (get-current-ice state)))
    (continue state :corp nil)))

(defn check-for-empty-server
  [state]
  (let [run (:run @state)
        server (first (:server run))]
    (and run
         (is-remote? server)
         (empty? (get-in @state [:corp :servers server :content]))
         (empty? (get-in @state [:corp :servers server :ices])))))

(defmethod start-next-phase :approach-ice
  [state side _]
  (set-phase state :approach-ice)
  (set-current-ice state)
  (reset-all-ice state side)
  (check-auto-no-action state)
  (let [eid (:eid (:run @state))
        ice (get-current-ice state)
        on-approach (:on-approach (card-def ice))]
    (system-msg state :runner (str "approaches " (card-str state ice)))
    (when on-approach
      (make-pending-event state :approach-ice ice on-approach))
    (queue-event state :approach-ice {:ice ice})
    (wait-for (checkpoint state nil
                          (make-eid state eid)
                          ;; Immediately end approach step if:
                          ;; * run ends
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (:ended (:run @state))
                                            (check-for-empty-server state)))})
              (if (get-in @state [:run :jack-out-after-pass])
                (wait-for (jack-out state :runner (make-eid state eid))
                          (when (or (check-for-empty-server state)
                                    (:ended (:run @state)))
                            (handle-end-run state side)))
                (when (or (check-for-empty-server state)
                          (:ended (:run @state)))
                  (handle-end-run state side))))))

(defmethod continue :approach-ice
  [state side _]
  (if-not (get-in @state [:run :no-action])
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :corp side)
          (system-msg state side "has no further action")))
    (let [eid (:eid (:run @state))
          ice (get-current-ice state)]
      (swap! state assoc-in [:run :jack-out] true)
      (wait-for (end-of-phase-checkpoint state nil (make-eid state eid) :end-of-approach-ice)
                (cond
                  (or (check-for-empty-server state)
                      (:ended (:run @state)))
                  (handle-end-run state side)
                  (rezzed? (get-current-ice state))
                  (do (set-next-phase state :encounter-ice)
                      (start-next-phase state :runner nil))
                  :else
                  (pass-ice state side))))))

(defn bypass-ice
  [state]
  (swap! state assoc-in [:run :bypass] true))

(defn can-bypass-ice
  [state side ice]
  (when-not (any-effects state side :bypass-ice false? ice)
    (:bypass (:run @state))))

(defn encounter-ends
  [state side]
  (swap! state assoc-in [:run :no-action] false)
  (let [eid (:eid (:run @state))
        ice (get-current-ice state)]
    (wait-for (end-of-phase-checkpoint state nil (make-eid state eid)
                                       :end-of-encounter
                                       {:ice ice})
              (when (get-in @state [:run :bypass])
                (system-msg state :runner (str "bypasses " (:title ice)))
                (swap! state dissoc-in [:run :bypass]))
              (cond
                (or (check-for-empty-server state)
                    (:ended (:run @state)))
                (handle-end-run state side)
                (not (get-in @state [:run :next-phase]))
                (pass-ice state side)))))

(defmethod start-next-phase :encounter-ice
  [state side _]
  (set-phase state :encounter-ice)
  (check-auto-no-action state)
  (let [eid (:eid (:run @state))
        ice (get-current-ice state)
        on-encounter (:on-encounter (card-def ice))
        current-server (:server (:run @state))]
    (system-msg state :runner (str "encounters " (card-str state ice)))
    (when on-encounter
      (make-pending-event state :encounter-ice ice on-encounter))
    (queue-event state :encounter-ice {:ice ice})
    (wait-for (checkpoint state side
                          (make-eid state eid)
                          ;; Immediately end encounter step if:
                          ;; * run ends
                          ;; * ice is not rezzed
                          ;; * ice is bypassed
                          ;; * run is moved to another server
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (:ended (:run @state))
                                            (can-bypass-ice state side (get-card state ice))
                                            (not (rezzed? (get-card state ice)))
                                            (not= current-server (:server (:run @state)))
                                            (check-for-empty-server state)))})
              (cond
                (or (check-for-empty-server state)
                    (:ended (:run @state)))
                (handle-end-run state side)
                (or (can-bypass-ice state side (get-card state ice))
                    (not (rezzed? (get-card state ice)))
                    (not= current-server (:server (:run @state))))
                (encounter-ends state side)))))

(defmethod continue :encounter-ice
  [state side {:keys [jack-out]}]
  (when (some? jack-out)
    ; TODO: Do not transmit this to the Corp (same with :no-action)
    (swap! state assoc-in [:run :jack-out-after-pass] jack-out))
  (if (or (get-in @state [:run :no-action])
          (get-in @state [:run :bypass]))
    (encounter-ends state side)
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :runner side)
          (system-msg state side "has no further action")))))

(defn pass-ice
  [state side]
  (let [pos (get-in @state [:run :position])
        ice (get-current-ice state)
        passed-all-ice (zero? (dec pos))
        current-server (:server (:run @state))
        eid (:eid (:run @state))]
    (set-phase state :pass-ice)
    (swap! state assoc-in [:run :no-action] false)
    (system-msg state :runner (str "passes " (card-str state ice)))
    (swap! state update-in [:run :position] (fnil dec 1))
    (queue-event state :pass-ice {:ice (get-card state ice)})
    (when passed-all-ice
      (queue-event state :pass-all-ice {:ice (get-card state ice)}))
    (wait-for (checkpoint state side
                          (make-eid state eid)
                          ;; Immediately end pass ice step if:
                          ;; * run ends
                          ;; * run is moved to another server
                          ;; * ice moves
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (:ended (:run @state))
                                            (not= current-server (:server (:run @state)))
                                            (not (same-card? ice (nth (get-run-ices state) (dec pos) nil)))
                                            (check-for-empty-server state)))})
              (reset-all-ice state side)
              (cond
                (or (check-for-empty-server state)
                    (:ended (:run @state)))
                (handle-end-run state side)
                (not (get-in @state [:run :next-phase]))
                (if (pos? (get-in @state [:run :position]))
                  (do (set-next-phase state :approach-ice)
                      (start-next-phase state side nil))
                  (do (set-next-phase state :approach-server)
                      (start-next-phase state side nil)))))))

(defmethod start-next-phase :pass-ice
  [state side _]
  (pass-ice state side))

(defmethod start-next-phase :approach-server
  [state side _]
  (let [eid (:eid (:run @state))
        no-ice? (zero? (count (get-run-ices state)))
        initiation-phase? (= :initiation (get-in @state [:run :phase]))]
    (set-current-ice state nil)
    (set-phase state :approach-server)
    (system-msg state :runner (str "approaches " (zone->name (:server (:run @state)))))
    (queue-event state :approach-server)
    (when (and no-ice? initiation-phase?)
      (queue-event state :pass-all-ice))
    (wait-for (checkpoint state side
                          (make-eid state eid)
                          ;; Immediately end pass ice step if:
                          ;; * run ends
                          ;; * position is no longer 0
                          ;; * server becomes empty
                          {:cancel-fn (fn [state]
                                        (or (:ended (:run @state))
                                            (pos? (:position (:run @state)))
                                            (check-for-empty-server state)))})
              (cond
                (or (check-for-empty-server state)
                    (:ended (:run @state)))
                (handle-end-run state side)
                (and (not (get-in @state [:run :next-phase]))
                     (get-in @state [:run :jack-out-after-pass]))
                (wait-for (jack-out state :runner (make-eid state))
                          (when (or (check-for-empty-server state)
                                    (:ended (:run @state)))
                            (handle-end-run state side)))))))

(defmethod continue :approach-server
  [state side _]
  (if-not (get-in @state [:run :no-action])
    (do (when (= :corp side) (system-msg state side "has no further action"))
        (swap! state assoc-in [:run :no-action] side))
    (do (if (get-in @state [:run :corp-phase-43])
          (set-next-phase state :corp-phase-43)
          (set-next-phase state :access-server))
        (start-next-phase state side nil))))

(defmethod start-next-phase :corp-phase-43
  [state _ _]
  (set-phase state :corp-phase-43)
  (system-msg state :corp "wants to act before the run is successful"))

(defmethod continue :corp-phase-43
  [state side _]
  (if-not (get-in @state [:run :no-action])
    (swap! state assoc-in [:run :no-action] side)
    (if-not (:ended (:run @state))
      (do (set-next-phase state :access-server)
          (start-next-phase state side nil))
      (handle-end-run state side))))

(defmethod start-next-phase :access-server
  [state side _]
  (set-phase state :access-server)
  (if (check-for-empty-server state)
    (handle-end-run state side)
    (successful-run state :runner)))

(defmethod continue :default
  [state _ _]
  (.println *err* (with-out-str
                    (print-stack-trace
                      (Exception. "Continue clicked at the wrong time")
                      2500))))

(defn redirect-run
  ([state side server] (redirect-run state side server nil))
  ([state side server phase]
   (let [dest (server->zone state server)
         num-ice (count (get-in (:corp @state) (conj dest :ices)))
         phase (if (= phase :approach-ice)
                 (if (pos? num-ice)
                   :approach-ice
                   :approach-server)
                 phase)]
     (swap! state update :run
            assoc
            :position num-ice
            :server [(second dest)])
     (when phase
       (set-next-phase state phase)))
   (set-current-ice state)))

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
                 :can-access (:can-access props)
                 :ability ability}]
    (swap! state update-in [:run :run-effects] conj ability)))

(defn successful-run-replace-access
  [props]
  (let [ability (:ability props)
        attacked-server (:target-server props)
        use-this-card-run (:this-card-run props)]
    {:event :successful-run
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
  (let [can-access (:can-access (:run @state))
        mandatory (some :mandatory handlers)
        titles (into [] (keep #(get-in % [:card :title]) handlers))]
    (cond
      ;; If you can't access, there's nothing to replace
      (not can-access)
      (run-cleanup state :runner)
      ;; Otherwise, if there's no handlers, access the cards
      (zero? (count titles))
      (wait-for (do-access state :runner (get-in @state [:run :server]))
                (handle-end-run state :runner))
      ;; If there's only 1 handler and it's mandatory
      ;; just execute it
      (and mandatory (= 1 (count titles)))
      (let [chosen (first handlers)
            ability (:ability chosen)
            card (:card chosen)]
        (system-msg state :runner (str "uses the replacement effect from " (:title card)))
        (wait-for (resolve-ability state :runner ability card [(select-keys (:run @state) [:server :run-id])])
                  (when-not (:can-access chosen)
                    (swap! state assoc-in [:run :can-access] false))
                  (choose-replacement-ability state nil)))
      ;; there are available handlers
      ;; checking for :can-access happens in :choices
      (pos? (count titles))
      (resolve-ability
        state :runner
        {:prompt "Choose an access replacement ability"
         :choices (if mandatory titles (conj titles "Access cards"))
         :effect (req (let [chosen (some #(when (= target (get-in % [:card :title])) %) handlers)
                            ability (:ability chosen)
                            card (:card chosen)]
                        (if chosen
                          (do (system-msg state :runner (str "uses the replacement effect from " (:title card)))
                              (wait-for (resolve-ability state :runner ability card [(select-keys (:run @state) [:server :run-id])])
                                        (when-not (:can-access chosen)
                                          (swap! state assoc-in [:run :can-access] false))
                                        (let [remaining (when (:can-access (:run @state))
                                                          (remove-once #(= target (get-in % [:card :title])) handlers))]
                                          (choose-replacement-ability state remaining))))
                          (do (system-msg state :runner "chooses to access cards instead of use a replacement effect")
                              (wait-for (do-access state :runner (get-in @state [:run :server]))
                                        (handle-end-run state :runner))))))}
        nil nil)
      ;; Just in case
      :else
      (run-cleanup state :runner))))

(defn prevent-access
  "Prevents the runner from accessing cards this run. This will cancel any run effects and not trigger access routines."
  [state _]
  (swap! state assoc-in [:run :prevent-access] true))

(defn complete-run
  "This does all of the access related stuff"
  [state side]
  (if (:ended (:run @state))
    (run-cleanup state :runner)
    (let [the-run (:run @state)
          server (:server the-run)
          replacement-effects (:run-effects the-run)]
      (cond
        ;; Prevented from accessing anything
        (:prevent-access the-run)
        (resolve-ability
          state :runner
          {:prompt "You are prevented from accessing any cards this run."
           :choices ["OK"]
           :effect (effect (system-msg :runner "is prevented from accessing any cards this run")
                           (handle-end-run))}
          nil nil)

        ;; Any number of replace-access effects
        (pos? (count replacement-effects))
        (choose-replacement-ability state replacement-effects)

        ;; No replace-access effects
        :else
        (wait-for (do-access state side server)
                  (handle-end-run state side))))))

(defn- register-successful-run
  [state side eid server]
  (swap! state update-in [:runner :register :successful-run] conj (first server))
  (swap! state assoc-in [:run :successful] true)
  ;; TODO: :pre-successful-run exists merely for Omar Keung and Sneakdoor Beta
  ;; Needs prevention system to remove
  (queue-event state :pre-successful-run (select-keys (:run @state) [:server :run-id]))
  (wait-for (checkpoint state nil (make-eid state eid))
            (queue-event state :successful-run (select-keys (:run @state) [:server :run-id]))
            (checkpoint state nil eid)))

(defn successful-run
  "The real 'successful run' trigger."
  [state side]
  (if (any-effects state side :block-successful-run)
    (complete-run state side)
    (wait-for (register-successful-run state side (make-eid state (:eid (:run @state))) (get-in @state [:run :server]))
              (complete-run state side))))

(defn corp-phase-43
  "The corp indicates they want to take action after runner hits Successful Run, before access."
  [state side _]
  (swap! state update-in [:run :corp-phase-43] not)
  (when-not (= :corp (get-in @state [:run :no-action]))
    (continue state side nil)))

(defn end-run-prevent
  [state _]
  (swap! state update-in [:end-run :end-run-prevent] (fnil inc 0)))

(defn- register-unsuccessful-run
  [state side eid]
  (let [run (:run @state)]
    (swap! state update-in [:runner :register :unsuccessful-run] conj (first (:server run)))
    (swap! state assoc-in [:run :unsuccessful] true)
    (handle-end-run state side)
    (queue-event state :unsuccessful-run run)
    (checkpoint state nil eid)))

(defn- resolve-end-run
  "End this run, and set it as UNSUCCESSFUL"
  ([state side eid]
   (if (get-in @state [:run :successful])
     (do (handle-end-run state side)
         (effect-completed state side eid))
     (register-unsuccessful-run state side eid))))

(defn end-run
  "After checking for prevents, end this run, and set it as UNSUCCESSFUL."
  ([state side eid card] (end-run state side eid card nil))
  ([state side eid card {:keys [unpreventable] :as args}]
   (swap! state update-in [:end-run] dissoc :end-run-prevent)
   (let [prevent (get-prevent-list state :runner :end-run)]
     (if (and (not unpreventable)
              (cards-can-prevent? state :runner prevent :end-run nil {:card-cause card}))
       (do (system-msg state :runner "has the option to prevent the run from ending")
           (show-wait-prompt state :corp "Runner to prevent the run from ending")
           (show-prompt state :runner nil
                        (str "Prevent the run from ending?") ["Done"]
                        (fn [_]
                          (clear-wait-prompt state :corp)
                          (if-let [_ (get-in @state [:end-run :end-run-prevent])]
                            (effect-completed state side eid)
                            (do (system-msg state :runner "will not prevent the run from ending")
                                (resolve-end-run state side eid))))))
       (resolve-end-run state side eid)))))

(defn jack-out-prevent
  [state side]
  (swap! state update-in [:jack-out :jack-out-prevent] (fnil inc 0))
  (prevent-jack-out state side))

(defn- resolve-jack-out
  [state side eid]
  (queue-event state :jack-out nil)
  (system-msg state side "jacks out")
  (end-run state side eid {:unpreventable true}))

(defn jack-out
  "The runner decides to jack out."
  ([state side eid]
   (swap! state update-in [:jack-out] dissoc :jack-out-prevent)
   (let [cost (jack-out-cost state side)]
     (if (can-pay? state side eid nil "jack out" cost)
       (wait-for (pay state :runner nil cost)
                 (if-let [payment-str (:msg async-result)]
                   (let [prevent (get-prevent-list state :corp :jack-out)]
                     (if (cards-can-prevent? state :corp prevent :jack-out)
                       (do (system-msg state :runner (str (build-spend-msg payment-str "attempt to" "attempts to") "jack out"))
                           (system-msg state :corp "has the option to prevent the Runner from jacking out")
                           (show-wait-prompt state :runner "Corp to prevent the jack out")
                           (show-prompt state :corp nil
                                        (str "Prevent the Runner from jacking out?") ["Done"]
                                        (fn [_]
                                          (clear-wait-prompt state :runner)
                                          (if-let [_ (get-in @state [:jack-out :jack-out-prevent])]
                                            (effect-completed state side (make-result eid false))
                                            (do (system-msg state :corp "will not prevent the Runner from jacking out")
                                                (resolve-jack-out state side eid))))))
                       (do (when-not (string/blank? payment-str)
                             (system-msg state :runner (str payment-str " to jack out")))
                           (resolve-jack-out state side eid))))
                   (complete-with-result state side eid false)))
       (do (system-msg state :runner (str "attempts to jack out but can't pay (" (build-cost-string cost) ")"))
           (complete-with-result state side eid false))))))

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
  [state side]
  (let [event (when (= :encounter-ice (get-in @state [:run :phase]))
                :end-of-encounter)]
    (queue-event state event {:ice (get-current-ice state)})
    (swap! state assoc-in [:run :ended] true)
    (let [run (:run @state)
          eid (:eid run)]
      (swap! state assoc-in [:runner :register :last-run] run)
      (swap! state update-in [:runner :credit] - (get-in @state [:runner :run-credit]))
      (swap! state assoc-in [:runner :run-credit] 0)
      (swap! state assoc :run nil)
      (queue-event state :run-ends run)
      (wait-for (checkpoint state nil (make-eid state eid) nil)
                (unregister-floating-effects state side :end-of-encounter)
                (unregister-floating-events state side :end-of-encounter)
                (unregister-floating-effects state side :end-of-run)
                (unregister-floating-events state side :end-of-run)
                (unregister-floating-effects state side :end-of-next-run)
                (unregister-floating-events state side :end-of-next-run)
                (reset-all-ice state side)
                (clear-run-register! state)
                (run-end-fx state side run)))))

(defn handle-end-run
  "Initiate run resolution."
  [state side]
  (if (and (empty? (get-in @state [:runner :prompt]))
           (empty? (get-in @state [:corp :prompt])))
    (run-cleanup state side)
    (swap! state assoc-in [:run :ended] true)))

(defn total-cards-accessed
  ([run]
   (apply + (vals (:cards-accessed run {}))))
  ([run server]
   (get-in run [:cards-accessed server] 0)))
