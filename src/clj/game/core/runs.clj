(in-ns 'game.core)

(declare any-flag-fn? clear-run-register! run-cleanup gain-run-credits
         update-ice-in-server update-all-ice get-agenda-points get-remote-names
         card-name can-access-loud can-steal?  prevent-jack-out card-flag? can-run?
         update-all-agenda-points reset-all-ice no-action make-run encounter-ends
         pass-ice do-access)

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
  (let [ice (get-in @state [:run :current-ice])]
    (or (get-card state ice) ice)))

(defn toggle-auto-no-action
  [state side args]
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
                                      :corp-auto-no-action false
                                      :jack-out false
                                      :jack-out-after-pass false
                                      :phase :initiation
                                      :next-phase :initiation
                                      :eid eid
                                      :current-ice nil
                                      :events nil})
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
  (check-auto-no-action state)
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
              (if (get-in @state [:run :jack-out-after-pass])
                (wait-for (jack-out state :runner (make-eid state))
                          (when (or (check-for-empty-server state)
                                    (:ended (:run @state)))
                            (handle-end-run state side)))
                (when (or (check-for-empty-server state)
                          (:ended (:run @state)))
                  (handle-end-run state side))))))

(defmethod continue :approach-ice
  [state side args]
  (if-not (get-in @state [:run :no-action])
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :corp side) (system-msg state side "has no further action")))
    (do (update-all-ice state side)
        (update-all-icebreakers state side)
        (swap! state assoc-in [:run :jack-out] true)
        (cond
          (or (check-for-empty-server state)
              (:ended (:run @state)))
          (handle-end-run state side)
          (rezzed? (get-current-ice state))
          (do (set-next-phase state :encounter-ice)
              (start-next-phase state :runner nil))
          :else
          (pass-ice state side)))))

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
  (check-auto-no-action state)
  (let [ice (get-current-ice state)
        on-encounter (when ice (:on-encounter (card-def ice)))
        current-server (:server (:run @state))]
    (system-msg state :runner (str "encounters " (card-str state ice)))
    (wait-for (trigger-event-simult state side :encounter-ice
                                    {:card-abilities (ability-as-handler ice on-encounter)
                                     ;; Immediately end encounter step if:
                                     ;; * run ends
                                     ;; * ice is not rezzed
                                     ;; * ice is bypassed
                                     ;; * run is moved to another server
                                     ;; * server becomes empty
                                     :cancel-fn (fn [state] (or (:ended (:run @state))
                                                                (can-bypass-ice state side (get-card state ice))
                                                                (not (rezzed? (get-card state ice)))
                                                                (not= current-server (:server (:run @state)))
                                                                (check-for-empty-server state)))}
                                    ice)
              (update-all-ice state side)
              (update-all-icebreakers state side)
              (cond
                (or (check-for-empty-server state)
                    (:ended (:run @state)))
                (handle-end-run state side)
                (or (can-bypass-ice state side (get-card state ice))
                    (not (rezzed? (get-card state ice)))
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
              (or (check-for-empty-server state)
                  (:ended (:run @state)))
              (handle-end-run state side)
              (not (get-in @state [:run :next-phase]))
              (pass-ice state side))))

(defmethod continue :encounter-ice
  [state side {:keys [jack-out] :as args}]
  (when (some? jack-out)
    (swap! state assoc-in [:run :jack-out-after-pass] jack-out)) ;ToDo: Do not transmit this to the Corp (same with :no-action)
  (if (or (get-in @state [:run :no-action])
          (get-in @state [:run :bypass]))
    (encounter-ends state side args)
    (do (swap! state assoc-in [:run :no-action] side)
        (when (= :runner side) (system-msg state side "has no further action")))))

(defn pass-ice
  [state side]
  (let [run-ice (get-run-ices state)
        pos (get-in @state [:run :position])
        ice (get-current-ice state)
        passed-all-ice (zero? (dec pos))
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
  [state side args]
  (pass-ice state side))

(defmethod start-next-phase :approach-server
  [state side args]
  (let [no-ice (zero? (count (get-run-ices state)))
        args (assoc
               (when (and no-ice
                          (= :initiation (get-in @state [:run :phase])))
                 {:card-abilities (gather-events state side :pass-all-ice nil)})
               ;; Immediately end pass ice step if:
               ;; * run ends
               ;; * position is no longer 0
               ;; * server becomes empty
               :cancel-fn (fn [state] (or (:ended (:run @state))
                                          (pos? (:position (:run @state)))
                                          (check-for-empty-server state))))]
        (set-current-ice state nil)
        (set-phase state :approach-server)
        (system-msg state :runner (str "approaches " (zone->name (:server (:run @state)))))
        (wait-for (trigger-event-simult state side :approach-server args (count (get-run-ices state)))
                  (update-all-ice state side)
                  (update-all-icebreakers state side)
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
  [state side args]
  (when-not (get-in @state [:run :no-action])
    (when (= :corp side) (system-msg state side "has no further action"))
    (swap! state assoc-in [:run :no-action] side)))

(defmethod continue :default
  [state side args]
  (.println *err* (with-out-str
                    (print-stack-trace
                      (Exception. "Continue clicked at the wrong time")
                      2500)))
  (.println *err* (str "Run: " (:run @state) "\n")))

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
   (set-current-ice state)
   (update-all-ice state side)
   (update-all-icebreakers state side)))

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

;;; Ending runs
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

(defn prevent-access
  "Prevents the runner from accessing cards this run. This will cancel any run effects and not trigger access routines."
  [state _]
  (swap! state assoc-in [:run :prevent-access] true))

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
  (swap! state assoc-in [:run :corp-phase-43] not)
  (continue state side nil))

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
                       (do (when (not (blank? cost-str)) (system-msg state :runner (str cost-str " to jack out")))
                           (resolve-jack-out state side eid)
                           (effect-completed state side (make-result eid false)))))
                   (effect-completed state side (make-result eid false))))
       (do (system-msg state :runner (str "attempts to jack out but can't pay (" (build-cost-string cost) ")"))
           (effect-completed state side (make-result eid false)))))))

(defn- run-end-fx
  [state side {:keys [eid successful unsuccessful]}]
  (cond
    ;; Successful
    successful
    (do
      (play-sfx state side "run-successful")
      (effect-completed state side (make-result eid {:successful true})))
    ;; Unsuccessful
    unsuccessful
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
              (run-end-fx state side run))))

(defn run-cleanup
  "Trigger appropriate events for the ending of a run."
  [state side]
  (let [server (-> @state :run :server first)
        event (when (= :encounter-ice (get-in @state [:run :phase])) :encounter-ice-ends)
        current-ice (when (= :encounter-ice (get-in @state [:run :phase]))
                      (or (get-current-ice state)
                          (get-in @state [:run :current-ice])))]
    (swap! state assoc-in [:run :ended] true)
    (wait-for (trigger-event-simult state side event nil current-ice)
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
