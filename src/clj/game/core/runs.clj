(in-ns 'game.core)

(declare any-flag-fn? clear-run-register! run-cleanup gain-run-credits
         update-ice-in-server update-all-ice get-agenda-points get-remote-names
         card-name can-access-loud can-steal?  prevent-jack-out card-flag? can-run?
         update-all-agenda-points reset-all-ice)

(defn add-run-effect
  [state side run-effect]
  (swap! state update-in [:run :run-effects] conj run-effect))

(defn total-run-cost
  ([state side card] (total-run-cost state side card nil))
  ([state side card {:keys [click-run ignore-costs] :as args}]
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
          additional-costs])))))

(declare make-run encounter-ends pass-ice)

(defn set-current-ice
  ([state]
   (let [run-ice (get-run-ices state)
         pos (get-in @state [:run :position])]
     (when (and pos
                (pos? pos)
                (<= pos (count run-ice)))
       (set-current-ice state (nth run-ice (dec pos))))))
  ([state card]
   (swap! state assoc-in [:run :current-ice] (get-card state card))))

(defn get-current-ice
  [state]
   (get-card state (get-in @state [:run :current-ice])))

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
  (fn [state side args]
    (:next-phase (:run @state))))

(defmulti continue
  (fn [state side args]
    (:phase (:run @state))))

(defn make-run
  "Starts a run on the given server, with the given card as the cause. If card is nil, assume a click was spent."
  ([state side server] (make-run state side (make-eid state) server nil nil nil))
  ([state side eid server] (make-run state side eid server nil nil nil))
  ([state side server run-effect card] (make-run state side (make-eid state) server run-effect card nil))
  ([state side eid server run-effect card] (make-run state side eid server run-effect card nil))
  ([state side eid server run-effect card {:keys [click-run ignore-costs] :as args}]
   (let [cost-args (assoc args :server (unknown->kw server))
         costs (total-run-cost state side card cost-args)]
     (if (and (can-run? state :runner)
              (can-run-server? state server)
              (can-pay? state :runner eid card "a run" costs))
       (do (when click-run
             (swap! state assoc-in [:runner :register :click-type] :run)
             (swap! state assoc-in [:runner :register :made-click-run] true)
             (play-sfx state side "click-run"))
           (wait-for (pay-sync state :runner (make-eid state {:source card :source-type :make-run}) nil costs)
                     (if-let [cost-str async-result]
                       (let [s [(if (keyword? server) server (last (server->zone state server)))]
                             ices (get-in @state (concat [:corp :servers] s [:ices]))
                             n (count ices)]
                         (when click-run
                           (system-msg state :runner (str (build-spend-msg cost-str "make a run on" "makes a run on")
                                                          (zone->name (unknown->kw server))
                                                          (when ignore-costs ", ignoring all costs"))))
                         ;; s is a keyword for the server, like :hq or :remote1
                         (swap! state assoc
                                :per-run nil
                                :run {:server s
                                      :position n
                                      :access-bonus []
                                      :jack-out false
                                      :phase :initiation
                                      :next-phase :initiation
                                      :eid eid})
                         (when (or run-effect card)
                           (add-run-effect state side (assoc run-effect :card card)))
                         (trigger-event state side :begin-run :server s)
                         (gain-run-credits state side (get-in @state [:runner :next-run-credit]))
                         (swap! state assoc-in [:runner :next-run-credit] 0)
                         (gain-run-credits state side (count-bad-pub state))
                         (swap! state update-in [:runner :register :made-run] #(conj % (first s)))
                         (swap! state update-in [:stats side :runs :started] (fnil inc 0))
                         (update-all-ice state side)
                         (update-all-icebreakers state side)
                         (wait-for (trigger-event-simult state :runner :run nil s n cost-args)
                                   (if (pos? (get-in @state [:run :position] 0))
                                     (do (set-next-phase state :approach-ice)
                                         (start-next-phase state side nil))
                                     (do (set-next-phase state :approach-server)
                                         (swap! state assoc-in [:run :jack-out] true)
                                         (start-next-phase state side nil)))))
                       (effect-completed state side eid))))
       (effect-completed state side eid)))))

(defn check-for-empty-server
  [state]
  (let [run (:run @state)
        server (first (:server run))]
    (and run
         (is-remote? server)
         (empty? (get-in @state [:corp :servers server :content]))
         (empty? (get-in @state [:corp :servers server :ices])))))

(defmethod start-next-phase :approach-ice
  [state side args]
  (set-phase state :approach-ice)
  (set-current-ice state)
  (update-all-ice state side)
  (update-all-icebreakers state side)
  (reset-all-ice state side)
  (let [ice (get-current-ice state)]
    (system-msg state :runner (str "approaches " (card-str state ice)))
    (wait-for (trigger-event-simult state :runner :approach-ice
                                    ;; Immediately end approach step if:
                                    ;; * run ends
                                    ;; * server becomes empty
                                    {:cancel-fn (fn [state] (or (:ended (:run @state))
                                                                (check-for-empty-server state)))}
                                    ice)
              (update-all-ice state side)
              (update-all-icebreakers state side)
              (when (:ended (:run @state))
                (handle-end-run state side)))))

(defmethod continue :approach-ice
  [state side args]
  (if (get-in @state [:run :no-action])
    (do (update-all-ice state side)
        (update-all-icebreakers state side)
        (swap! state assoc-in [:run :no-action] false)
        (swap! state assoc-in [:run :jack-out] true)
        (cond
          (:ended (:run @state))
          (handle-end-run state side)
          (rezzed? (get-current-ice state))
          (do (set-next-phase state :encounter-ice)
              (start-next-phase state side nil))
          :else
          (pass-ice state side)))
    (do (swap! state assoc-in [:run :no-action] :runner)
        (system-msg state side "has no further action"))))

(defn bypass-ice
  [state]
  (swap! state assoc-in [:run :bypass] true))

(defn can-bypass-ice
  [state side ice]
  (when-not (any-effects state side :bypass-ice false? ice)
    (:bypass (:run @state))))

(defmethod start-next-phase :encounter-ice
  [state side args]
  (set-phase state :encounter-ice)
  (update-all-ice state side)
  (update-all-icebreakers state side)
  (let [ice (get-current-ice state)
        on-encounter (when ice (:on-encounter (card-def ice)))
        current-server (:server (:run @state))]
    (system-msg state :runner (str "encounters " (card-str state ice)))
    (wait-for (trigger-event-simult state side :encounter-ice
                                    {:card-abilities (ability-as-handler ice on-encounter)
                                     ;; Immediately end encounter step if:
                                     ;; * run ends
                                     ;; * ice is bypassed
                                     ;; * run is moved to another server
                                     ;; * server becomes empty
                                     :cancel-fn (fn [state] (or (:ended (:run @state))
                                                                (can-bypass-ice state side (get-card state ice))
                                                                (not= current-server (:server (:run @state)))
                                                                (check-for-empty-server state)))}
                                    ice)
              (update-all-ice state side)
              (update-all-icebreakers state side)
              (cond
                (:ended (:run @state))
                (handle-end-run state side)
                (or (can-bypass-ice state side (get-card state ice))
                    (not= current-server (:server (:run @state))))
                (encounter-ends state side args)))))

(defn encounter-ends
  [state side args]
  (update-all-ice state side)
  (update-all-icebreakers state side)
  (swap! state assoc-in [:run :no-action] false)
  (wait-for (trigger-event-simult state :runner :encounter-ice-ends nil (get-current-ice state))
            (swap! state dissoc-in [:run :bypass])
            (unregister-floating-effects state side :end-of-encounter)
            (unregister-floating-events state side :end-of-encounter)
            (update-all-ice state side)
            (update-all-icebreakers state side)
            (cond
              (:ended (:run @state))
              (handle-end-run state side)
              (not (get-in @state [:run :next-phase]))
              (pass-ice state side))))

(defmethod continue :encounter-ice
  [state side args]
  (if (or (get-in @state [:run :no-action])
          (get-in @state [:run :bypass]))
    (encounter-ends state side args)
    (do (swap! state assoc-in [:run :no-action] :runner)
        (system-msg state side "has no further action"))))

(defn pass-ice
  [state side]
  (let [run-ice (get-run-ices state)
        pos (get-in @state [:run :position])
        ice (get-current-ice state)
        passed-all-ice (and (pos? (count run-ice))
                            (zero? (dec pos)))
        current-server (:server (:run @state))
        args (assoc
               (when passed-all-ice
                 {:card-abilities (gather-events state side :pass-all-ice nil)})
               ;; Immediately end pass ice step if:
               ;; * run ends
               ;; * run is moved to another server
               ;; * server becomes empty
               :cancel-fn (fn [state] (or (:ended (:run @state))
                                          (not= current-server (:server (:run @state)))
                                          (check-for-empty-server state))))]
    (set-phase state :pass-ice)
    (update-all-ice state side)
    (update-all-icebreakers state side)
    (swap! state assoc-in [:run :no-action] false)
    (system-msg state :runner (str "passes " (card-str state ice)))
    (swap! state update-in [:run :position] (fnil dec 1))
    (wait-for (trigger-event-simult state side :pass-ice args ice)
              (update-all-ice state side)
              (update-all-icebreakers state side)
              (reset-all-ice state side)
              (cond
                (:ended (:run @state))
                (handle-end-run state side)
                (not (get-in @state [:run :next-phase]))
                (if (pos? (get-in @state [:run :position]))
                  (do (set-next-phase state :approach-ice)
                      (start-next-phase state side nil))
                  (do (set-next-phase state :approach-server)
                      (start-next-phase state side nil)))))))

(defmethod continue :pass-ice
  [state side args]
  (.println *err* (with-out-str
                    (print-stack-trace
                      (Exception. "Continue clicked at the wrong time")
                      2500)))
  (.println *err* (str "Run: " (:run @state) "\n")))

(defmethod start-next-phase :approach-server
  [state side args]
  (let [no-ice (zero? (count (get-run-ices state)))
        args (assoc
               (when (and no-ice
                          (= :initiation (get-in @state [:run :phase])))
                 {:card-abilities (gather-events state side :pass-all-ice nil)})
               ;; Immediately end pass ice step if:
               ;; * run ends
               ;; * server becomes empty
               :cancel-fn (fn [state] (or (:ended (:run @state))
                                          (check-for-empty-server state))))]
        (set-phase state :approach-server)
        (system-msg state :runner (str "approaches " (zone->name (:server (:run @state)))))
        (wait-for (trigger-event-simult state side :approach-server args (count (get-run-ices state)))
                  (update-all-ice state side)
                  (update-all-icebreakers state side)
                  (when (:ended (:run @state))
                    (handle-end-run state side)))))

(defmethod continue :approach-server
  [state side args] nil)

(defn no-action
  "The corp indicates they have no more actions for the encounter."
  [state side args]
  (if (get-in @state [:run :no-action])
    (continue state :runner args)
    (do (swap! state assoc-in [:run :no-action] :corp)
        (system-msg state side "has no further action"))))

;; Non timing stuff
(defn gain-run-credits
  "Add temporary credits that will disappear when the run is over."
  [state side n]
  (swap! state update-in [:runner :run-credit] (fnil + 0 0) n)
  (gain-credits state :runner n))

(defn gain-next-run-credits
  "Add temporary credits for the next run to be initiated."
  [state side n]
  (swap! state update-in [:runner :next-run-credit] (fnil + 0 0) n))

(defn no-trash-or-steal
  [state]
  (swap! state update-in [:runner :register :no-trash-or-steal] (fnil inc 0)))

(defn access-end
  "Trigger events involving the end of the access phase, including :no-trash and :post-access-card"
  [state side eid c]
  ;; Do not trigger :no-trash if card has already been trashed
  (wait-for (trigger-event-sync state side
                                (when-not (find-cid (:cid c) (get-in @state [:corp :discard]))
                                  :no-trash)
                                c)
            (wait-for (trigger-event-sync state side
                                          (when (and (agenda? c)
                                                     (not (find-cid (:cid c) (get-in @state [:runner :scored]))))
                                            :no-steal)
                                          c)
                      (when (and (get-card state c)
                                 ;; Don't increment :no-trash-or-steal if accessing a card in Archives
                                 (not= (:zone c) [:discard]))
                        (no-trash-or-steal state))
                      (swap! state dissoc :access)
                      (trigger-event-sync state side eid :post-access-card c))))

;;; Stealing agendas
(defn steal
  "Moves a card to the runner's :scored area, triggering events from the completion of the steal."
  ([state side card] (steal state side (make-eid state) card))
  ([state side eid card]
   (let [c (move state :runner (dissoc card :advance-counter :new) :scored {:force true})
         points (get-agenda-points state :runner c)]
     (wait-for
       (trigger-event-simult
         state :runner :agenda-stolen
         {:first-ability {:effect (req (system-msg state :runner (str "steals " (:title c) " and gains "
                                                                      (quantify points "agenda point")))
                                       (swap! state update-in [:runner :register :stole-agenda]
                                              #(+ (or % 0) (:agendapoints c 0)))
                                       (update-all-agenda-points state side)
                                       (check-winner state side)
                                       (play-sfx state side "agenda-steal")
                                       (when (:run @state)
                                         (swap! state assoc-in [:run :did-steal] true))
                                       (when (card-flag? c :has-events-when-stolen true)
                                         (register-events state side c))
                                       (remove-old-current state side :corp))}
          :card-abilities (ability-as-handler c (:stolen (card-def c)))}
         c)
       (access-end state side eid card)))))

(defn- steal-agenda
  "Trigger the stealing of an agenda, now that costs have been paid."
  ([state side card] (steal-agenda state side (make-eid state) card))
  ([state side eid card]
   (let [cdef (card-def card)]
     (if (or (not (:steal-req cdef)) ((:steal-req cdef) state :runner (make-eid state) card nil))
       (steal state :runner eid card)
       (access-end state side eid card)))))

(defn steal-cost-bonus
  "Applies a cost to the next steal attempt. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :steal-cost] #(merge-costs (concat % costs))))

(defn steal-cost
  "Gets a vector of costs for stealing the given agenda."
  [state side card]
  (-> (when-let [costfun (:steal-cost-bonus (card-def card))]
        (costfun state side (make-eid state) card nil))
      (concat (get-in @state [:bonus :steal-cost]))
      merge-costs
      flatten
      vec))

;;; Accessing rules.
(defn interactions
  [card ability-key]
  (get-in (card-def card) [:interactions ability-key]))

(defn- access-ab
  [card]
  (interactions card :access-ability))

(defn- access-ab-label
  [card]
  (let [title (first (string/split (:title card) #":"))
        label (make-label (access-ab card))]
    (str "[" title "] " label)))

(defn access-non-agenda
  "Access a non-agenda. Show a prompt to trash for trashable cards."
  [state side eid c & {:keys [skip-trigger-event]}]
  (when-not skip-trigger-event
    (trigger-event state side :pre-trash c))
  (swap! state update-in [:stats :runner :access :cards] (fnil inc 0))
  ; Don't show the access prompt if:
  (if (or ; 1) accessing cards in Archives
          (in-discard? c)
          ; 2) Edward Kim's auto-trash flag is true
          (and (operation? c)
               (card-flag? c :can-trash-operation true))
          ; 3) card has already been trashed but hasn't been updated
          (find-cid (:cid c) (get-in @state [:corp :discard])))
    (access-end state side eid c)
    ; Otherwise, show the access prompt
    (let [card (assoc c :seen true)
          ; Trash costs
          trash-cost (trash-cost state side card)
          trash-eid (assoc eid :source card :source-type :runner-trash-corp-cards)
          can-pay (when trash-cost
                    (can-pay? state :runner trash-eid card nil [:credit trash-cost]))
          trash-cost-str (when can-pay
                           [(str "Pay " trash-cost " [Credits] to trash")])
          ; Is the runner is forced to trash this card with only credits? (NAT)
          must-trash-with-credits? (and can-pay
                                        (get-in @state [:runner :register :must-trash-with-credits]))
          ; Access abilities
          access-ab-cards (when-not must-trash-with-credits?
                            (seq (filter #(can-trigger? state :runner (access-ab %) % [card])
                                         (all-active state :runner))))
          ; Remove any non-trash abilities, as they can't be used if we're forced to trash
          trash-ab-cards (seq (filter #(:trash? (access-ab %) true) access-ab-cards))
          ; Is the runner is forced to trash this card by any means?
          ; Only relevant when not forced to trash with credits, as we want to include
          ; trash abilities here
          must-trash? (when-not must-trash-with-credits?
                        (and (or can-pay trash-ab-cards)
                             (card-flag-fn? state side card :must-trash true)))
          ; If we must trash, make the label only from the trash abilities
          ; Otherwise, make the label from all abilities
          ability-strs (mapv access-ab-label
                             (if must-trash? trash-ab-cards access-ab-cards))
          ; Only display "No action" when we're not forced to do anything
          no-action-str (when-not (or must-trash? must-trash-with-credits?)
                          ["No action"])
          choices (vec (concat ability-strs trash-cost-str no-action-str))]
      (continue-ability
        state :runner
        {:async true
         :prompt (str "You accessed " (:title card) ".")
         :choices choices
         :effect (req (cond
                        ; Can't or won't trash or use an ability
                        (= target (first no-action-str))
                        (access-end state side eid c)

                        ; Pay credits (from pool or cards) to trash
                        (= target (first trash-cost-str))
                        (wait-for (pay-sync state side (make-eid state trash-eid) card [:credit trash-cost])
                                  (when (:run @state)
                                    (swap! state assoc-in [:run :did-trash] true)
                                    (when must-trash?
                                      (swap! state assoc-in [:run :did-access] true)))
                                  (swap! state assoc-in [:runner :register :trashed-card] true)
                                  (system-msg state side (str async-result " to trash "
                                                              (:title card) " from "
                                                              (name-zone :corp (get-nested-zone card))))
                                  (wait-for (trash state side card nil)
                                            (access-end state side eid c)))

                        ; Use access ability
                        (some #(= % target) ability-strs)
                        (let [idx (.indexOf ability-strs target)
                              ability-card (nth access-ab-cards idx)
                              ability-eid (assoc eid :source ability-card :source-type :ability)
                              ability (access-ab ability-card)]
                          (when (and (:run @state)
                                     (:trash? ability true))
                            (swap! state assoc-in [:run :did-trash] true))
                          (wait-for (resolve-ability state side (make-eid state ability-eid) ability ability-card [card])
                                    (access-end state side eid c)))))}
        card nil))))

(defn- access-agenda
  "Rules interactions for a runner that has accessed an agenda and may be able to steal it."
  [state side eid card]
  (trigger-event state side :pre-steal-cost card)
  (swap! state update-in [:stats :runner :access :cards] (fnil inc 0))
  (let [cost (steal-cost state side card)
        part-cost (partition 2 cost)
        cost-strs (build-cost-string cost)
        can-pay (can-pay? state side (make-eid state eid) card (:title card) cost)
        can-steal (can-steal? state side card)
        ; Access abilities are useless in the discard
        access-ab-cards (when-not (in-discard? card)
                          (seq (filter #(can-trigger? state :runner (access-ab %) % [card])
                                       (all-active state :runner))))
        ability-strs (mapv access-ab-label access-ab-cards)
        ;; strs
        steal-str (when (and can-steal can-pay)
                    (if (not (string/blank? cost-strs))
                      ["Pay to steal"]
                      ["Steal"]))
        no-action-str (when-not (= steal-str ["Steal"])
                        ["No action"])
        prompt-str (if (not (string/blank? cost-strs))
                     (str " " cost-strs " to steal?")
                     "")
        prompt-str (str "You accessed " (:title card) "." prompt-str)
        choices (vec (concat ability-strs steal-str no-action-str))]
    ;; Steal costs are additional costs and can be denied by the runner.
    (continue-ability
      state :runner
      {:async true
       :prompt prompt-str
       :choices choices
       :effect (req (cond
                      ;; Can't steal or pay, or won't pay additional costs to steal
                      (= target "No action")
                      (do (when-not (find-cid (:cid card) (:deck corp))
                            (system-msg state side (str "decides to not pay to steal " (:title card))))
                          (access-end state side eid card))

                      ;; Steal normally
                      (= target "Steal")
                      (steal-agenda state side eid card)

                      ;; Pay additiional costs to steal
                      (= target "Pay to steal")
                      (wait-for (pay-sync state side nil cost {:action :steal-cost})
                                (system-msg state side (str async-result " to steal "
                                                            (:title card) " from "
                                                            (name-zone :corp (get-nested-zone card))))
                                (steal-agenda state side eid card))

                      ;; Use access ability
                      (some #(= % target) ability-strs)
                      (let [idx (.indexOf ability-strs target)
                            ability-card (nth access-ab-cards idx)
                            ability-eid (assoc eid :source ability-card :source-type :ability)
                            ability (access-ab ability-card)]
                        (when (and (:run @state)
                                   (:trash? ability true))
                          (swap! state assoc-in [:run :did-trash] true))
                        (wait-for (resolve-ability state side (make-eid state ability-eid) ability ability-card [card])
                                  (trigger-event state side :no-steal card)
                                  (access-end state side eid card)))))}
      card nil)))

(defn- reveal-access?
  "Check if the card should be revealed on access"
  ;; If any special reveal message is wanted it can go in this function
  [state side {:keys [zone] :as card}]
  (let [cdef (card-def card)
        ;; Add more kw here as the maybe become relevant. Only think rd is relevant,
        ;; everything else should not be "unseen".
        reveal-kw (match (vec zone)
                         [:deck] :rd-reveal
                         [:hand] :hq-reveal
                         [:discard] :archives-reveal
                         :else :reveal)]
    ;; Check if the zone-reveal keyword exists in the flags property of the card definition
    (when-let [reveal-fn (get-in cdef [:flags reveal-kw])]
      (reveal-fn state side (make-eid state) card nil))))

(defn- join-cost-strs
  [& costs]
  (->> costs
       flatten
       (filter some?)
       (interpose " and ")
       (apply str)))

(defn msg-handle-access
  "Generate the message from the access"
  [state side {:keys [zone] :as card} title cost-msg]
  (let [cost-str (join-cost-strs cost-msg)]
    (system-msg state side
                (str (if (seq cost-msg)
                       (str cost-str " to access ")
                       "accesses ")
                     title
                     (when card
                       (str " from " (name-zone side zone))))))
  (when (reveal-access? state side card)
    (system-msg state side (str "must reveal they accessed " (:title card)))
    (reveal state :runner card)))

(defn- access-trigger-events
  "Trigger access effects, then move into trash/steal choice."
  [state side eid c title & cost-msg]
  (let [cdef (card-def c)
        c (assoc c :seen true)
        access-effect (when-let [acc (:access cdef)]
                        (ability-as-handler c acc))]
    (swap! state assoc-in [:runner :register :accessed-cards] true)
    (msg-handle-access state side c title cost-msg)
    (wait-for (trigger-event-simult state side :access
                                    {:card-abilities access-effect
                                     ; Cancel other access handlers if the card moves zones because of a handler
                                     ; or access has been stopped
                                     :cancel-fn (fn [state] (or (not (get-card state c))
                                                                (not (:access @state))))}
                                    c)
              ; make sure the card has not been moved by a handler
              ; and we're still accessing the card
              (if (and (get-card state c)
                       (same-card? c (:access @state)))
                (if (agenda? c)
                  (access-agenda state side eid c)
                  ;; Accessing a non-agenda
                  (access-non-agenda state side eid c))
                (access-end state side eid c)))))

(defn access-cost-bonus
  "Applies a cost to the next access. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :access-cost] #(merge-costs (concat % costs))))

(defn access-cost
  "Gets a vector of costs for accessing the given card."
  [state side card]
  (merge-costs (get-in @state [:bonus :access-cost])))

(defn- access-pay
  "Force the runner to pay any costs to access this card, if any, before proceeding with access."
  [state side eid card title]
  (let [cost (access-cost state side card)
        cost-str (build-cost-string cost)
        can-pay (when (not-empty cost)
                  (can-pay? state side (make-eid state eid) nil nil cost))
        prompt-str (if can-pay
                     (str cost-str " to access this card?")
                     "You can't pay the cost to access this card.")
        choices (if can-pay
                  ["Pay to access" "No action"]
                  ["OK"])]
    (cond
      ;; Did a pre-access-card effect trash the card? (By Any Means)
      (not (get-card state card))
      (access-end state side eid card)
      ;; There are access costs
      (not-empty cost)
      (continue-ability
        state :runner
        {:async true
         :prompt prompt-str
         :choices choices
         :effect (req (cond
                        (= "OK" target)
                        (access-end state side eid card)
                        (= "No action" target)
                        (access-end state side eid card)
                        :else
                        (wait-for (pay-sync state side card cost)
                                  (if async-result
                                    (access-trigger-events state side eid card title async-result)
                                    (access-end state side eid card)))))}
        card nil)
      ;; There are no access costs
      :else
      (access-trigger-events state side eid card title))))

(defn access-card
  "Apply game rules for accessing the given card."
  ([state side card] (access-card state side (make-eid state) card nil))
  ([state side eid card] (access-card state side eid card (:title card)))
  ([state side eid card title]
    ;; Indicate that we are in the access step.
   (swap! state assoc :access card)
    ;; Reset counters for increasing costs of trash, steal, and access.
   (swap! state update-in [:bonus] dissoc :trash)
   (swap! state update-in [:bonus] dissoc :steal-cost)
   (swap! state update-in [:bonus] dissoc :access-cost)
   (when (:run @state)
     (let [zone (or (#{:discard :deck :hand} (-> card :zone first))
                    (-> card :zone second))]
       (swap! state update-in [:run :cards-accessed zone] (fnil inc 0))))
   ;; First trigger pre-access-card, then move to determining if we can trash or steal.
   (wait-for (trigger-event-sync state side :pre-access-card card)
             (access-pay state side eid card title))))

(defn prevent-access
  "Prevents the runner from accessing cards this run. This will cancel any run effects and not trigger access routines."
  [state _]
  (swap! state assoc-in [:run :prevent-access] true))

(defn max-access
  "Put an upper limit on the number of cards that can be accessed in this run. For Eater."
  [state side n]
  (swap! state assoc-in [:run :max-access] n))

(defn access-bonus
  "Increase the number of cards to be accessed in server during this run by n.
  For temporary/per-run effects like Legwork, Maker's Eye.
  Not for permanent increases like RDI."
  [state side server bonus]
  (swap! state update-in [:run :access-bonus] conj [server bonus]))

(defn access-bonus-count
  [run s]
  (reduce
    (fn [acc [server bonus]]
      (if (= s server)
        (+ acc bonus)
        acc))
    0
    (:access-bonus run)))

(defn access-count
  [state side kw]
  (let [run (:run @state)
        s (case kw
           :rd-access :rd
           :hq-access :hq
           kw)
        accesses (+ (get-in @state [:runner kw])
                    (access-bonus-count run s))]
    (if-let [max-access (:max-access run)]
      (min max-access accesses)
      accesses)))


;;; Methods for allowing user-controlled multi-access in servers.

;; choose-access implements game prompts allowing the runner to choose the order of access.
(defmulti choose-access (fn [cards server args] (get-server-type (first server))))

(defn access-helper-remote [cards]
  {:prompt "Click a card to access it. You must access all cards in this server."
   :choices {:card #(some (fn [c] (same-card? % c)) cards)}
   :async true
   :effect (req (wait-for (access-card state side target)
                          (if (< 1 (count cards))
                            (continue-ability state side (access-helper-remote (remove #(same-card? % target) cards))
                                              card nil)
                            (effect-completed state side eid))))})

(defmethod choose-access :remote [cards server args]
  {:async true
   :effect (req (cond
                  ;; Only 1 card
                  (= 1 (count cards))
                  (access-card state side eid (first cards))
                  ;; Normal access
                  (pos? (count cards))
                  (continue-ability state side (access-helper-remote cards) card nil)
                  :else
                  (effect-completed state side eid)))})

(defn access-helper-hq-or-rd
  "Shows a prompt to access card(s) from the given zone.
  zone: :rd or :hq, for finding Upgrades to access.
  label: a string label to describe what is being accessed, e.g., 'Card from deck' -- 'deck' being the label.
  amount: how many accesses the runner has remaining.
  select-fn: a function taking the already-accessed set as argument, and returning the next card to access
      from the given zone.
  title-fn: a function taking a card map being accessed and returning a string to print as the card's title, e.g.,
      'an unseen card from R&D' for an R&D run.
  already-accessed: a set of cards already accessed from this zone or its root."
  [state chosen-zone label amount select-fn title-fn already-accessed]
  (let [get-root-content (fn [state]
                           (remove #(contains? already-accessed %) (get-in @state [:corp :servers chosen-zone :content])))
        server-name (central->name chosen-zone)
        unrezzed-upgrade (str "Unrezzed upgrade in " server-name)
        card-from (str "Card from " label)]
    {:async true
     :prompt "Select a card to access."
     :choices (concat (when (pos? amount) [card-from])
                      (map #(if (rezzed? %) (:title %) unrezzed-upgrade)
                           (get-root-content state)))
     :effect (req (cond
                    (= target unrezzed-upgrade)
                    ;; accessing an unrezzed upgrade
                    (let [from-root (get-root-content state)
                          unrezzed (filter #(and (= (last (:zone %)) :content) (not (rezzed? %)))
                                           from-root)]
                      (if (= 1 (count unrezzed))
                        ;; only one unrezzed upgrade; access it and continue
                        (wait-for (access-card state side (first unrezzed))
                                  (if (or (pos? amount) (< 1 (count from-root)))
                                    (continue-ability
                                      state side
                                      (access-helper-hq-or-rd state chosen-zone label amount select-fn title-fn
                                                              (conj already-accessed (first unrezzed)))
                                      card nil)
                                    (effect-completed state side eid)))
                        ;; more than one unrezzed upgrade. allow user to select with mouse.
                        (continue-ability
                          state side
                          {:async true
                           :prompt (str "Choose an upgrade in " server-name " to access.")
                           :choices {:card #(and (= (second (:zone %)) chosen-zone)
                                                 (complement already-accessed))}
                           :effect (req (wait-for (access-card state side target)
                                                  (continue-ability
                                                    state side
                                                    (access-helper-hq-or-rd state chosen-zone label amount select-fn title-fn
                                                                            (conj already-accessed target))
                                                    card nil)))}
                          card nil)))
                    ;; accessing a card in deck
                    (= target card-from)
                    (let [accessed (select-fn already-accessed)]
                      (wait-for (access-card state side (make-eid state) accessed
                                             (title-fn accessed))

                                (let [from-root (get-root-content state)]
                                  (if (or (< 1 amount) (not-empty from-root))
                                    (continue-ability
                                      state side
                                      (access-helper-hq-or-rd state chosen-zone label (dec amount) select-fn title-fn
                                                              (if (-> @state :run :shuffled-during-access chosen-zone)
                                                                ;; if the zone was shuffled because of the access,
                                                                ;; the runner "starts over" excepting any upgrades that were accessed
                                                                (do (swap! state update-in [:run :shuffled-during-access] dissoc chosen-zone)
                                                                    (set (filter #(= :servers (first (:zone %)))
                                                                                 already-accessed)))
                                                                (conj already-accessed accessed)))
                                      card nil)
                                    (effect-completed state side eid)))))
                    ;; accessing a rezzed upgrade
                    :else
                    (let [accessed (some #(when (= (:title %) target) %) (get-root-content state))]
                      (wait-for (access-card state side accessed)
                                (if (or (pos? amount)
                                        (< 1 (count (get-root-content state))))
                                  (continue-ability
                                    state side
                                    (access-helper-hq-or-rd state chosen-zone label amount select-fn title-fn
                                                            (conj already-accessed accessed))
                                    card nil)
                                  (effect-completed state side eid))))))}))

(defn- access-cards-from-rd
  [state]
  (let [f (get-in @state [:runner :rd-access-fn])]
    (f (get-in @state [:corp :deck]))))

(defmethod choose-access :rd [cards server {:keys [no-root] :as args}]
  {:async true
   :effect (req (cond
                  ;; Only 1 card
                  (= 1 (count cards))
                  (access-card state side eid (first cards) "an unseen card")
                  ;; Normal access
                  (pos? (count cards))
                  (let [from-rd (access-count state side :rd-access)]
                    (continue-ability state side (access-helper-hq-or-rd
                                                   state :rd "deck" from-rd
                                                   ;; access the first card in deck that has not been accessed.
                                                   (fn [already-accessed] (first (drop-while already-accessed
                                                                                             (access-cards-from-rd state))))
                                                   (fn [_] "an unseen card")
                                                   (if no-root
                                                     (set (get-in @state [:corp :servers :rd :content]))
                                                     #{}))
                                      card nil))
                  :else
                  (effect-completed state side eid)))})

(defn access-helper-hq
  "This is a helper for cards to invoke HQ access without knowing how to use the full access method. See Dedicated Neural Net."
  [state from-hq already-accessed]
  (access-helper-hq-or-rd state :hq "hand" from-hq
                          (fn [already-accessed] (some #(when-not (already-accessed %) %)
                                                       (shuffle (-> @state :corp :hand))))
                          :title
                          already-accessed))

(defmethod choose-access :hq [cards server {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [cards-count (count cards)]
                  (cond
                    ;; Corp chooses accessed cards
                    (and (pos? cards-count)
                         (any-effects state side :corp-choose-hq-access))
                    (do (show-wait-prompt state :runner "Corp to select cards in HQ to be accessed")
                        (continue-ability
                          state :corp
                          {:prompt (msg "Select " (min (access-count state side :hq-access)
                                                       (-> @state :corp :hand count)) " cards in HQ for the Runner to access")
                           :choices {:card #(and (in-hand? %) (corp? %))
                                     :all true
                                     :max (req (min (access-count state side :hq-access)
                                                    (-> @state :corp :hand count)))}
                           :async true
                           :effect (req (clear-wait-prompt state :runner)
                                        (if (= 1 cards-count)
                                          (access-card state side eid target)
                                          (continue-ability
                                            state :runner
                                            (access-helper-hq
                                              state (access-count state side :hq-access)
                                              ; access-helper-hq uses a set to keep track of which cards have already
                                              ; been accessed. Using the set difference we make the runner unable to
                                              ; access non-selected cards from the corp prompt
                                              (clojure.set/difference (set (:hand corp)) (set targets)))
                                            card nil)))}
                          card nil))
                    ;; Only 1 card
                    (= 1 cards-count)
                    (access-card state side eid (first cards))
                    ;; Normal access
                    (pos? cards-count)
                    (let [from-hq (min (access-count state side :hq-access)
                                       (-> @state :corp :hand count))
                          ; Handle root only access - no cards to access in hand
                          from-hq (if (some in-hand? cards) from-hq 0)]
                      (continue-ability state side (access-helper-hq-or-rd
                                                     state :hq "hand" from-hq
                                                     (fn [already-accessed] (some #(when-not (already-accessed %) %)
                                                                                  (shuffle (-> @state :corp :hand))))
                                                     (fn [card] (:title card))
                                                     (if no-root
                                                       (set (get-in @state [:corp :servers :hq :content]))
                                                       #{}))
                                        card nil))
                    :else
                    (effect-completed state side eid))))})

(defn- get-archives-accessible [state]
  ;; only include agendas and cards with an :access ability whose :req is true
  ;; (or don't have a :req, or have an :optional with no :req, or :optional with a true :req.)
  (filter #(let [cdef (card-def %)]
             ;; must also be :seen
             (and (:seen %)
                  (or (agenda? %)
                      (should-trigger? state :corp % nil (:access cdef)))))
          (get-in @state [:corp :discard])))

(defn- get-archives-inactive [state]
  ;; get faceup cards with no access interaction
  (filter #(let [cdef (card-def %)]
             (and (:seen %)
                  (not (or (agenda? %)
                           (should-trigger? state :corp % nil (:access cdef))))))
          (get-in @state [:corp :discard])))

(defn access-helper-archives [state amount already-accessed]
  (let [root-content (fn [already-accessed] (remove already-accessed (-> @state :corp :servers :archives :content)))
        faceup-accessible (fn [already-accessed] (remove already-accessed (get-archives-accessible state)))
        facedown-cards (fn [already-accessed] (filter #(and (not (:seen %))
                                                            (not (already-accessed %)))
                                                      (-> @state :corp :discard)))

        next-access (fn [state side eid already-accessed card]
                      (continue-ability state side (access-helper-archives state (dec amount) already-accessed)
                                        card nil))

        must-continue? (fn [already-accessed]
                         (and (< 1 amount)
                              (pos? (+ (count (root-content already-accessed))
                                       (count (faceup-accessible already-accessed))
                                       (count (facedown-cards already-accessed))))))]
    {:async true
     :prompt "Select a card to access. You must access all cards."
     :choices (concat (when (<= amount (count (filter (complement already-accessed) (get-archives-inactive state))))
                        [(str "Access " amount " inactive cards")])
                      (map :title (faceup-accessible already-accessed))
                      (map #(if (rezzed? %) (:title %) "Unrezzed upgrade in Archives") (root-content already-accessed))
                      (map (fn [_] (str "Facedown card in Archives")) (facedown-cards already-accessed)))
     :effect (req (cond
                    (.endsWith target "inactive cards")
                    ;; Interaction with Bacterial Programming. If we have X accesses remaining and <= X inactive cards
                    ;; in Archives, we don't have to access the remaining active cards.  This only happens if you choose
                    ;; to access at least one of the facedown cards added to Archives by Bacterial Programming.
                    (do (system-msg state side "accesses the remaining inactive cards in Archives")
                        (effect-completed state side eid))

                    (= target "Facedown card in Archives")
                    ;; accessing a card that was added to archives because of the effect of another card
                    (let [accessed (first (shuffle (facedown-cards already-accessed)))
                          already-accessed (conj already-accessed accessed)]
                      (wait-for (access-card state side accessed)
                                (if (must-continue? already-accessed)
                                  (next-access state side eid already-accessed card)
                                  (effect-completed state side eid))))

                    (= target "Unrezzed upgrade in Archives")
                    ;; accessing an unrezzed upgrade
                    (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (rezzed? %)))
                                           (root-content already-accessed))]
                      (if (= 1 (count unrezzed))
                        ;; only one unrezzed upgrade; access it and continue
                        (let [already-accessed (conj already-accessed (first unrezzed))]
                          (wait-for (access-card state side (first unrezzed))
                                    (if (must-continue? already-accessed)
                                      (next-access state side eid already-accessed card)
                                      (effect-completed state side eid))))
                        ;; more than one unrezzed upgrade. allow user to select with mouse.
                        (continue-ability
                          state side
                          {:async true
                           :prompt "Choose an upgrade in Archives to access."
                           :choices {:card #(and (= (second (:zone %)) :archives)
                                                 (not (already-accessed %)))}
                           :effect (req (let [already-accessed (conj already-accessed target)]
                                          (wait-for (access-card state side target)
                                                    (if (must-continue? already-accessed)
                                                      (next-access state side eid already-accessed card)
                                                      (effect-completed state side eid)))))}
                          card nil)))

                    :else
                    ;; accessing a rezzed upgrade, or a card in archives
                    (let [accessed (some #(when (= (:title %) target) %)
                                         (concat (faceup-accessible already-accessed) (root-content already-accessed)))
                          already-accessed (conj already-accessed accessed)]
                      (wait-for (access-card state side accessed)
                                (if (must-continue? already-accessed)
                                  (next-access state side eid already-accessed card)
                                  (effect-completed state side eid))))))}))

(defmethod choose-access :archives [cards server {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [archives-count (count cards)
                      active-cards (concat (get-archives-accessible state) (-> @state :corp :servers :archives :content))]
                  ;; Because we don't "access" cards in Archives like normal,
                  ;; we have to manually count all the cards we'd normally skip
                  (when (and (:run @state)
                             (not (safe-zero? (get-in @state [:run :max-access]))))
                    (swap! state update-in [:run :cards-accessed :discard] (fnil + 0 0) (- archives-count (count active-cards))))
                  (if (and (seq cards)
                           (seq active-cards))
                    (if (= 1 (count active-cards))
                      (access-card state side eid (first active-cards))
                      (continue-ability state side
                                        (access-helper-archives state archives-count
                                                                (if no-root
                                                                  (set (get-in @state [:corp :servers :archives :content]))
                                                                  #{}))
                                        card nil))
                    (do (system-msg state side (str "accesses " archives-count " cards in Archives"))
                        (effect-completed state side eid)))))})

(defn get-all-hosted [hosts]
  (let [hosted-cards (mapcat :hosted hosts)]
    (if (empty? hosted-cards)
      hosted-cards
      (concat hosted-cards (get-all-hosted hosted-cards)))))


(defmulti cards-to-access
  "Gets the list of cards to access for the server"
  (fn [state side server] (get-server-type (first server))))

(defmethod cards-to-access :hq [state side server]
  (concat (take (access-count state side :hq-access) (shuffle (get-in @state [:corp :hand])))
          (get-in @state [:corp :servers :hq :content])))

(defmethod cards-to-access :rd [state side server]
  (concat (take (access-count state side :rd-access) (access-cards-from-rd state))
          (get-in @state [:corp :servers :rd :content])))

(defmethod cards-to-access :archives [state side server]
  (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
  (when-not (safe-zero? (get-in @state [:run :max-access]))
    (concat (get-in @state [:corp :discard]) (get-in @state [:corp :servers :archives :content]))))

(defmethod cards-to-access :remote [state side server]
  (let [contents (get-in @state [:corp :servers (first server) :content])]
    (when-not (safe-zero? (get-in @state [:run :max-access]))
      (filter (partial can-access-loud state side) (concat contents (get-all-hosted contents))))))

(defn set-cards-to-access
  "Currently only used with Ash 2X"
  [state side & cards]
  (swap! state update-in [:run :cards-to-access] concat cards))

(defn get-cards-to-access
  "Currently only used with do-access below and Top Hat. It's hacky but whatever"
  [state]
  (seq (filter identity (map #(get-card state %) (get-in @state [:run :cards-to-access])))))

(defn do-access
  "Starts the access routines for the run's server."
  ([state side server] (do-access state side (make-eid state) server))
  ([state side eid server] (do-access state side eid server nil))
  ([state side eid server {:keys [hq-root-only no-root] :as args}]
   (wait-for (trigger-event-sync state side :pre-access (first server))
             (let [cards (cards-to-access state side server)
                   cards (if hq-root-only (remove #(= '[:hand] (:zone %)) cards) cards)
                   cards (if no-root (remove #(or (= '[:servers :rd :content] (:zone %))
                                                  (= '[:servers :hq :content] (:zone %))) cards) cards)
                   cards (or (get-cards-to-access state) cards)
                   n (count cards)]
               (swap! state dissoc :cards-to-access)
               ;; Make `:did-access` true when reaching the access step (no replacement)
               (when (:run @state)
                 (swap! state assoc-in [:run :did-access] true))
               (when-not (or (zero? n)
                             (safe-zero? (get-in @state [:run :max-access])))
                 (swap! state assoc-in [:runner :register :accessed-cards] true))
               (wait-for (resolve-ability state side (choose-access cards server args) nil nil)
                         (wait-for (trigger-event-sync state side :end-access-phase {:from-server (first server)})
                                   (unregister-floating-effects state side :end-of-access)
                                   (unregister-floating-events state side :end-of-access)
                                   (effect-completed state side eid)))))))

;;; Ending runs.
(defn register-successful-run
  ([state side server] (register-successful-run state side (make-eid state) server))
  ([state side eid server]
   (swap! state update-in [:runner :register :successful-run] #(conj % (first server)))
   (swap! state assoc-in [:run :successful] true)
   (wait-for (trigger-event-simult state side :pre-successful-run nil (first server))
             (wait-for (trigger-event-simult state side :successful-run nil (first (get-in @state [:run :server])))
                       (wait-for (trigger-event-simult state side :post-successful-run nil (first (get-in @state [:run :server])))
                                 (effect-completed state side eid))))))

(defn replace-access
  "Replaces the standard access routine with the :replace-access effect of the card"
  [state side ability card]
  (wait-for (resolve-ability state side ability card nil)
            (run-cleanup state side)))

(defn successful-run-effect-impl
  [state side eid run-effects]
  (if-let [run-effect (first run-effects)]
    (wait-for (resolve-ability state side (when-not (trigger-suppress state side :successful-run (:card run-effect))
                                            (:successful-run run-effect))
                               (:card run-effect) nil)
              (successful-run-effect-impl state side eid (next run-effects)))
    (effect-completed state side eid)))

(defn- successful-run-trigger
  "The real 'successful run' trigger."
  [state side]
  (wait-for
    (successful-run-effect-impl state side (filter :successful-run (get-in @state [:run :run-effects])))
    (wait-for (register-successful-run state side (get-in @state [:run :server]))
              (if (:ended (:run @state))
                (run-cleanup state :runner)
                (let [the-run (:run @state)
                      server (:server the-run) ; bind here as the server might have changed
                      run-effects (->> (:run-effects the-run)
                                       (filter #(and (:replace-access %)
                                                     (or (not (:req %))
                                                         ((:req %) state :runner (:eid the-run) (:card %) [(first server)]))))
                                       doall)
                      mandatory-run-effects (->> run-effects
                                                 (filter #(get-in % [:replace-access :mandatory]))
                                                 doall)]
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

                    ;; One mandatory replace-access effect
                    (= 1 (count mandatory-run-effects))
                    (let [chosen (first mandatory-run-effects)]
                      (system-msg state :runner (str "must use the replacement effect from " (:title (:card chosen))))
                      (replace-access state :runner (:replace-access chosen) (:card chosen)))

                    ;; Multiple mandatory replace-access effects
                    (pos? (count mandatory-run-effects))
                    (resolve-ability
                      state :runner
                      {:prompt "Choose a mandatory replacement effect"
                       :choices (mapv #(get-in % [:card :title]) mandatory-run-effects)
                       :effect (req (let [chosen (some #(when (= target (get-in % [:card :title])) %) mandatory-run-effects)]
                                      (system-msg state :runner
                                                  (str "chooses to use the replacement effect from " (:title (:card chosen))))
                                      (replace-access state :runner (:replace-access chosen) (:card chosen))))}
                      nil nil)

                    ;; Any number of optional replace-access effects
                    (pos? (count run-effects))
                    (resolve-ability
                      state :runner
                      {:prompt "Use a replacement effect instead of accessing cards?"
                       :choices (conj (mapv #(get-in % [:card :title]) run-effects) "Access cards")
                       :effect (req (if-let [chosen (some #(when (= target (get-in % [:card :title])) %) run-effects)]
                                      (do (system-msg state :runner
                                                      (str "chooses to use the replacement effect from " (:title (:card chosen))))
                                          (replace-access state :runner (:replace-access chosen) (:card chosen)))
                                      (do (system-msg state :runner "chooses to access cards instead of use a replacement effect")
                                          (wait-for (do-access state :runner server)
                                                    (handle-end-run state :runner)))))}
                      nil nil)

                    ;; No replace-access effects
                    :else
                    (wait-for (do-access state side server)
                              (handle-end-run state side))))))))

(defn successful-run
  "Run when a run has passed all ice and the runner decides to access. The corp may still get to act in 4.3."
  [state side args]
  (if (get-in @state [:run :corp-phase-43])
    ;; if corp requests phase 4.3, then we do NOT fire :successful-run yet, which does not happen until 4.4
    (do (swap! state dissoc :no-action)
        (system-msg state :corp "wants to act before the run is successful")
        (show-wait-prompt state :runner "Corp's actions")
        (show-prompt state :corp nil "Rez and take actions before Successful Run" ["Done"]
                     (fn [args-corp]
                       (clear-wait-prompt state :runner)
                       (if-not (:ended (:run @state))
                        (show-prompt state :runner nil "The run is now successful" ["Continue"]
                                     (fn [args-runner] (successful-run-trigger state :runner)))
                        (handle-end-run state side)))
                     {:priority -1}))
    (successful-run-trigger state side)))

(defn corp-phase-43
  "The corp indicates they want to take action after runner hits Successful Run, before access."
  [state side args]
  (swap! state assoc-in [:run :corp-phase-43] true)
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (trigger-event state side :no-action))

(defn end-run-prevent
  [state side]
  (swap! state update-in [:end-run :end-run-prevent] (fnil inc 0)))

(defn- resolve-end-run
  "End this run, and set it as UNSUCCESSFUL"
  ([state side eid]
   (if (get-in @state [:run :successful])
     (do (handle-end-run state side)
         (effect-completed state side eid))
     (let [run (:run @state)
           server (first (get-in @state [:run :server]))]
       (swap! state update-in [:runner :register :unsuccessful-run] #(conj % server))
       (swap! state assoc-in [:run :unsuccessful] true)
       (handle-end-run state side)
       (trigger-event-sync state side eid :unsuccessful-run run)))))

(defn end-run
  "After checking for prevents, end this run, and set it as UNSUCCESSFUL."
  ([state side eid card]
   (swap! state update-in [:end-run] dissoc :end-run-prevent)
   (let [prevent (get-prevent-list state :runner :end-run)]
     (if (cards-can-prevent? state :runner prevent :end-run nil {:card-cause card})
       (do (system-msg state :runner "has the option to prevent the run from ending")
           (show-wait-prompt state :corp "Runner to prevent the run from ending" {:priority 10})
           (show-prompt state :runner nil
                        (str "Prevent the run from ending?") ["Done"]
                        (fn [_]
                          (clear-wait-prompt state :corp)
                          (if-let [_ (get-in @state [:end-run :end-run-prevent])]
                            (effect-completed state side eid)
                            (do (system-msg state :runner "will not prevent the run from ending")
                                (resolve-end-run state side eid))))
                        {:priority 10}))
       (resolve-end-run state side eid)))))

(defn jack-out-prevent
  [state side]
  (swap! state update-in [:jack-out :jack-out-prevent] (fnil inc 0))
  (prevent-jack-out state side))

(defn- resolve-jack-out
  [state side eid]
  (wait-for (end-run state side nil)
            (system-msg state side "jacks out")
            (wait-for (trigger-event-sync state side :jack-out)
                      (complete-with-result state side eid true))))

(defn jack-out
  "The runner decides to jack out."
  ([state side eid]
   (swap! state update-in [:jack-out] dissoc :jack-out-prevent)
   (let [cost (jack-out-cost state side)]
     (if (can-pay? state side eid nil "jack out" cost)
       (wait-for (pay-sync state :runner nil cost)
                 (if-let [cost-str async-result]
                   (let [prevent (get-prevent-list state :corp :jack-out)]
                     (if (cards-can-prevent? state :corp prevent :jack-out)
                       (do (system-msg state :runner (str (build-spend-msg cost-str "attempt to" "attempts to") "jack out"))
                           (system-msg state :corp "has the option to prevent the Runner from jacking out")
                           (show-wait-prompt state :runner "Corp to prevent the jack out" {:priority 10})
                           (show-prompt state :corp nil
                                        (str "Prevent the Runner from jacking out?") ["Done"]
                                        (fn [_]
                                          (clear-wait-prompt state :runner)
                                          (if-let [_ (get-in @state [:jack-out :jack-out-prevent])]
                                            (effect-completed state side (make-result eid false))
                                            (do (system-msg state :corp "will not prevent the Runner from jacking out")
                                                (resolve-jack-out state side eid))))
                                        {:priority 10}))
                       (do (system-msg state :runner (str cost-str " to jack out"))
                           (resolve-jack-out state side eid)
                           (effect-completed state side (make-result eid false)))))
                   (effect-completed state side (make-result eid false))))
       (do (system-msg state :runner (str "attempts to jack out but can't pay (" (build-cost-string cost) ")"))
           (effect-completed state side (make-result eid false)))))))

(defn- run-end-fx
  [state side eid run]
  (cond
    ;; Successful
    (:successful run)
    (do
      (play-sfx state side "run-successful")
      (effect-completed state side (make-result eid {:successful true})))
    ;; Unsuccessful
    (:unsuccessful run)
    (do
      (play-sfx state side "run-unsuccessful")
      (effect-completed state side (make-result eid {:unsuccessful true})))
    ;; Neither
    :else
    (effect-completed state side (make-result eid nil))))

(defn run-cleanup-2
  [state side]
  (let [run (:run @state)]
    (swap! state assoc-in [:runner :register :last-run] run)
    (swap! state update-in [:runner :credit] - (get-in @state [:runner :run-credit]))
    (swap! state assoc-in [:runner :run-credit] 0)
    (swap! state assoc :run nil)
    (wait-for (trigger-event-simult state side :run-ends nil run)
              (unregister-floating-effects state side :end-of-run)
              (unregister-floating-events state side :end-of-run)
              (update-all-icebreakers state side)
              (update-all-ice state side)
              (reset-all-ice state side)
              (clear-run-register! state)
              (run-end-fx state side (:eid run) run))))

(defn run-cleanup
  "Trigger appropriate events for the ending of a run."
  [state side]
  (let [server (-> @state :run :server first)
        event (when (= :encounter-ice (get-in @state [:run :phase])) :encounter-ice-ends)]
    (swap! state assoc-in [:run :ended] true)
    (wait-for (trigger-event-simult state side event nil (get-current-ice state))
              (unregister-floating-effects state side :end-of-encounter)
              (unregister-floating-events state side :end-of-encounter)
              (run-cleanup-2 state side))))

(defn handle-end-run
  "Initiate run resolution."
  [state side]
  (if (and (empty? (get-in @state [:runner :prompt]))
           (empty? (get-in @state [:corp :prompt])))
    (run-cleanup state side)
    (swap! state assoc-in [:run :ended] true)))

(defn close-access-prompt
  "Closes a 'You accessed _' prompt through a non-standard card effect like Imp."
  [state side]
  (let [prompt (-> @state side :prompt first)
        eid (:eid prompt)]
    (swap! state update-in [side :prompt] rest)
    (effect-completed state side eid)
    (when-let [run (:run @state)]
      (when (and (:ended run)
                 (empty? (get-in @state [:runner :prompt])))
        (handle-end-run state :runner)))))

(defn get-run-ices
  [state]
  (get-in @state (concat [:corp :servers] (:server (:run @state)) [:ices])))

(defn total-cards-accessed
  ([run]
   (apply + (vals (:cards-accessed run {}))))
  ([run server]
   (get-in run [:cards-accessed server] 0)))
