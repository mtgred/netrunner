(in-ns 'game.core)

(declare clear-run-register!
         gain-run-credits update-ice-in-server update-all-ice
         get-agenda-points gain-agenda-point optional-ability
         get-remote-names card-name)

;;; Steps in the run sequence
(defn run
  "Starts a run on the given server, with the given card as the cause."
  ([state side server] (run state side server nil nil))
  ([state side server run-effect card]
   (when-not (get-in @state [:runner :register :cannot-run])
     (let [s [(if (keyword? server) server (last (server->zone state server)))]
           ices (get-in @state (concat [:corp :servers] s [:ices]))]
       ;; s is a keyword for the server, like :hq or :remote1
       (swap! state assoc :per-run nil
              :run {:server s :position (count ices) :ices ices :access-bonus 0
                    :run-effect (assoc run-effect :card card)})
       (gain-run-credits state side (get-in @state [:corp :bad-publicity]))
       (swap! state update-in [:runner :register :made-run] #(conj % (first s)))
       (update-all-ice state :corp)
       (trigger-event state :runner :run s)))))

(defn gain-run-credits
  "Add temporary credits that will disappear when the run is over."
  [state side n]
  (swap! state update-in [:runner :run-credit] + n)
  (gain state :runner :credit n))

;;; Stealing agendas
(defn steal
  "Moves a card to the runner's :scored area, triggering events from the completion of the steal."
  [state side card]
  (let [c (move state :runner (dissoc card :advance-counter) :scored)
        points (get-agenda-points state :runner c)]
    (resolve-ability state :runner (:stolen (card-def c)) c nil)
    (system-msg state :runner (str "steals " (:title c) " and gains " points
                                   " agenda point" (when (> points 1) "s")))
    (swap! state update-in [:runner :register :stole-agenda] #(+ (or % 0) points))
    (gain-agenda-point state :runner points)
    (when-let [current (first (get-in @state [:corp :current]))]
      (say state side {:user "__system__" :text (str (:title current) " is trashed.")})
      (trash state side current))
    (trigger-event state :runner :agenda-stolen c)))

(defn resolve-steal-events
  "Trigger events from accessing an agenda, which were delayed to account for Film Critic."
  [state side c]
  (let [cdef (card-def c)]
    (when-let [access-effect (:access cdef)]
      (resolve-ability state (to-keyword (:side c)) access-effect c nil))
    (trigger-event state side :access c)))

(defn resolve-steal
  "Finish the stealing of an agenda."
  [state side c]
  (let [cdef (card-def c)]
    (resolve-steal-events state side c)
    (when (or (not (:steal-req cdef)) ((:steal-req cdef) state :runner c nil))
      (steal state :runner c))))

(defn steal-cost-bonus
  "Applies a cost to the next steal attempt. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :steal-cost] #(merge-costs (concat % costs))))

(defn steal-cost
  "Gets a vector of costs for stealing the given agenda."
  [state side card]
  (-> (when-let [costfun (:steal-cost-bonus (card-def card))]
        (costfun state side card nil))
      (concat (get-in @state [:bonus :steal-cost]))
      merge-costs flatten vec))

;;; Accessing rules.
(defn access-cost-bonus
  "Applies a cost to the next access. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :access-cost] #(merge-costs (concat % costs))))

(defn access-cost
  "Gets a vector of costs for accessing the given card."
  [state side card]
  (-> (when-let [costfun (:access-cost-bonus (card-def card))]
        (costfun state side card nil))
      (concat (get-in @state [:bonus :access-cost]))
      merge-costs flatten vec))

(defn handle-access
  "Apply game rules for accessing the given list of cards (which generally only contains 1 card.)"
  [state side cards]
  (swap! state assoc :access true)
  (doseq [c cards]
    ;; Reset counters for increasing costs of trash, steal, and access.
    (swap! state update-in [:bonus] dissoc :trash)
    (swap! state update-in [:bonus] dissoc :steal-cost)
    (swap! state update-in [:bonus] dissoc :access-cost)
    (trigger-event state side :pre-access-card c)
    (let [acost (access-cost state side c)
          ;; hack to prevent toasts when playing against Gagarin and accessing on 0 credits
          anon-card (dissoc c :title)]
      (if (or (empty? acost) (pay state side anon-card acost))
        ;; Either there were no access costs, or the runner cold pay them.
        (let [cdef (card-def c)
              c (assoc c :seen true)]
          (when-let [name (:title c)]
            (if (is-type? c "Agenda") ; accessing an agenda
              (do (trigger-event state side :pre-steal-cost c)
                  (if (get-in @state [:runner :register :cannot-steal])
                    ;; The runner cannot steal this agenda.
                    (do (resolve-steal-events state side c)
                        (prompt! state :runner c (str "You accessed but cannot steal " (:title c)) ["OK"] {}))
                    ;; The runner can potentially steal this agenda.
                    (let [cost (steal-cost state side c)]
                      ;; Steal costs are additional costs and can be denied by the runner.
                      (if-not (empty? cost)
                        ;; Ask if the runner will pay the additional cost to steal.
                        (optional-ability state :runner c (str "Pay " (costs-to-symbol cost) " to steal " name "?")
                                          {:yes-ability
                                           {:effect (req (if (can-pay? state side name cost)
                                                           (do (pay state side nil cost)
                                                               (system-msg state side (str "pays " (costs-to-symbol cost)
                                                                                           " to steal " name))
                                                               (resolve-steal state side c))
                                                           (resolve-steal-events state side c)))}
                                           :no-ability {:effect (effect (resolve-steal-events c))}} nil)
                        ;; Otherwise, show the "You access" prompt with the single option to Steal.
                        (resolve-ability state :runner
                                         {:prompt (str "You access " name) :choices ["Steal"]
                                          :effect (req (resolve-steal state :runner c))} c nil)))))
              ;; Accessing a non-agenda
              (do (when-let [access-effect (:access cdef)]
                    (resolve-ability state (to-keyword (:side c)) access-effect c nil))
                  (trigger-event state side :access c)
                  (trigger-event state side :pre-trash c)
                  (when (not= (:zone c) [:discard]) ; if not accessing in Archives
                    (if-let [trash-cost (trash-cost state side c)]
                      ;; The card has a trash cost (Asset, Upgrade)
                      (let [card (assoc c :seen true)]
                        (if (and (get-in @state [:runner :register :force-trash])
                                 (can-pay? state :runner name :credit trash-cost))
                          ;; If the runner is forced to trash this card (Neutralize All Threats)
                          (resolve-ability state :runner {:cost [:credit trash-cost]
                                                          :effect (effect (trash card)
                                                                          (system-msg (str "is forced to pay " trash-cost
                                                                                           " [Credits] to trash " (:title card))))} card nil)
                          ;; Otherwise, show the option to pay to trash the card.
                          (optional-ability state :runner card (str "Pay " trash-cost "[Credits] to trash " name "?")
                                            {:yes-ability {:cost [:credit trash-cost]
                                                           :effect (effect (trash card)
                                                                           (system-msg (str "pays " trash-cost " [Credits] to trash "
                                                                                            (:title card))))}} nil)))
                      ;; The card does not have a trash cost
                      (prompt! state :runner c (str "You accessed " (:title c)) ["OK"] {})))))))
        ;; The runner cannot afford the cost to access the card
        (prompt! state :runner nil "You can't pay the cost to access this card" ["OK"] {})))))

(defn max-access
  "Put an upper limit on the number of cards that can be accessed in this run. For Eater."
  [state side n]
  (swap! state assoc-in [:run :max-access] n))

(defn access-bonus
  "Increase the number of cards to be accessed during this run by n. Legwork, Maker's Eye.
  Not for permanent increases like RDI."
  [state side n]
  (swap! state update-in [:run :access-bonus] #(+ % n)))

(defn access-count [state side kw]
  (let [run (:run @state)
        accesses (+ (get-in @state [:runner kw]) (:access-bonus run))]
    (if-let [max-access (:max-access run)]
      (min max-access accesses) accesses)))


;;; Methods for allowing user-controlled multi-access in servers.

;; choose-access implements game prompts allowing the runner to choose the order of access.
(defmulti choose-access (fn [cards server] (get-server-type (first server))))

(defn access-helper-remote [cards]
  {:prompt "Click a card to access it. You must access all cards in this server."
   :choices {:req #(some (fn [c] (= (:cid %) (:cid c))) cards)}
   :effect (req (handle-access state side [target])
                (when (< 1 (count cards))
                  (resolve-ability state side (access-helper-remote (filter #(not= (:cid %) (:cid target)) cards))
                                   card nil)))})

(defmethod choose-access :remote [cards server]
  {:effect (req (if (>= 1 (count cards))
                  (handle-access state side cards)
                  (resolve-ability state side (access-helper-remote cards) card nil)))})

(defn access-helper-hq [cards]
  {:prompt "Select a card to access."
   :choices (concat (when (some #(= (first (:zone %)) :hand) cards) ["Card from hand"])
                    (map #(if (rezzed? %) (:title %) "Unrezzed upgrade in HQ")
                         (filter #(= (last (:zone %)) :content) cards)))
   :effect (req (case target
                  "Unrezzed upgrade in HQ"
                  ;; accessing an unrezzed upgrade
                  (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %))) cards)]
                    (if (= 1 (count unrezzed))
                      ;; only one unrezzed upgrade; access it and continue
                      (do (system-msg state side (str "accesses " (:title (first unrezzed))))
                          (handle-access state side unrezzed)
                          (when (< 1 (count cards))
                            (resolve-ability
                              state side (access-helper-hq (filter #(not= (:cid %) (:cid (first unrezzed))) cards))
                              card nil)))
                      ;; more than one unrezzed upgrade. allow user to select with mouse.
                      (resolve-ability state side
                                       {:prompt "Choose an upgrade in HQ to access."
                                        :choices {:req #(= (second (:zone %)) :hq)}
                                        :effect (effect (system-msg (str "accesses " (:title target)))
                                                        (handle-access [target])
                                                        (resolve-ability (access-helper-hq
                                                                           (remove-once #(not= (:cid %) (:cid target))
                                                                                        cards)) card nil))} card nil)))
                  ;; accessing a card in hand or a rezzed upgade
                  "Card from hand"
                  (do (system-msg state side (str "accesses " (:title (first cards))))
                      (handle-access state side [(first cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-hq (rest cards)) card nil)))
                  ;; accessing a rezzed upgrade
                  (do (system-msg state side (str "accesses " target))
                      (handle-access state side [(some #(when (= (:title %) target) %) cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-hq
                                                      (remove-once #(not= (:title %) target) cards)) card nil)))))})

(defmethod choose-access :hq [cards server]
  {:effect (req (if (pos? (count cards))
                  (if (= 1 (count cards))
                    (do (when (pos? (count cards)) (system-msg state side (str "accesses " (:title (first cards)))))
                        (handle-access state side cards))
                    (resolve-ability state side (access-helper-hq cards) card nil))))})

(defn access-helper-rd [cards]
  {:prompt "Select a card to access."
   :choices (concat (when (some #(= (first (:zone %)) :deck) cards) ["Card from deck"])
                    (map #(if (rezzed? %) (:title %) "Unrezzed upgrade in R&D")
                         (filter #(= (last (:zone %)) :content) cards)))
   :effect (req (case target
                  "Unrezzed upgrade in R&D"
                  ;; accessing an unrezzed upgrade
                  (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %))) cards)]
                    (if (= 1 (count unrezzed))
                      ;; only one unrezzed upgrade; access it and continue
                      (do (handle-access state side unrezzed)
                          (when (< 1 (count cards))
                            (resolve-ability
                              state side (access-helper-rd (filter #(not= (:cid %) (:cid (first unrezzed))) cards))
                              card nil)))
                      ;; more than one unrezzed upgrade. allow user to select with mouse.
                      (resolve-ability state side
                                       {:prompt "Choose an upgrade in R&D to access."
                                        :choices {:req #(= (second (:zone %)) :rd)}
                                        :effect (effect (handle-access [target])
                                                        (resolve-ability (access-helper-rd
                                                                           (remove-once #(not= (:cid %) (:cid target))
                                                                                        cards)) card nil))} card nil)))
                  ;; accessing a card in deck or a rezzed upgade
                  "Card from deck"
                  (do (handle-access state side [(first cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-rd (rest cards)) card nil)))
                  ;; accessing a rezzed upgrade
                  (do (handle-access state side [(some #(when (= (:title %) target) %) cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-rd
                                                      (remove-once #(not= (:title %) target) cards)) card nil)))))})

(defmethod choose-access :rd [cards server]
  {:effect (req (if (pos? (count cards))
                  (if (= 1 (count cards))
                    (handle-access state side cards)
                    (resolve-ability state side (access-helper-rd cards) card nil))))})

(defn access-helper-archives [cards]
  {:prompt "Select a card to access. You must access all cards."
   :choices (map #(if (= (last (:zone %)) :content)
                   (if (rezzed? %) (:title %) "Unrezzed upgrade in Archives")
                   (:title %)) cards)
   :effect (req (case target
                  "Unrezzed upgrade in Archives"
                  ;; accessing an unrezzed upgrade
                  (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %))) cards)]
                    (if (= 1 (count unrezzed))
                      ;; only one unrezzed upgrade; access it and continue
                      (do (system-msg state side (str "accesses " (:title (first unrezzed))))
                          (handle-access state side unrezzed)
                          (when (< 1 (count cards))
                            (resolve-ability
                              state side (access-helper-archives (filter #(not= (:cid %) (:cid (first unrezzed))) cards))
                              card nil)))
                      ;; more than one unrezzed upgrade. allow user to select with mouse.
                      (resolve-ability state side
                                       {:prompt "Choose an upgrade in Archives to access."
                                        :choices {:req #(= (second (:zone %)) :archives)}
                                        :effect (effect (system-msg (str "accesses " (:title target)))
                                                        (handle-access [target])
                                                        (resolve-ability (access-helper-archives
                                                                           (remove-once #(not= (:cid %) (:cid target))
                                                                                        cards)) card nil))} card nil)))
                  ;; accessing a rezzed upgrade, or a card in archives
                  (do (system-msg state side (str "accesses " target))
                      (handle-access state side [(some #(when (= (:title %) target) %) cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-archives
                                                      (remove-once #(not= (:title %) target) cards)) card nil)))))})

(defmethod choose-access :archives [cards server]
  {:effect (req (let [; only include agendas and cards with an :access ability whose :req is true
                      ; (or don't have a :req, or have an :optional with no :req, or :optional with a true :req.)
                      cards (filter #(let [cdef (card-def %)]
                                      (or (is-type? % "Agenda")
                                          (= (last (:zone %)) :content)
                                          (and (:access cdef)
                                               (not (get-in cdef [:access :optional]))
                                               (or (not (get-in cdef [:access :req]))
                                                   ((get-in cdef [:access :req]) state side % nil)))
                                          (and (get-in cdef [:access :optional])
                                               (or (not (get-in cdef [:access :optional :req]))
                                                   ((get-in cdef [:access :optional :req]) state side % nil)))))
                                    cards)]
                  (if (pos? (count cards))
                    (if (= 1 (count cards))
                      (do (when (pos? (count cards)) (system-msg state side (str "accesses " (:title (first cards)))))
                          (handle-access state side cards))
                      (resolve-ability state side (access-helper-archives cards) card nil)))))})


;; Don't call access directly; use handle-access instead.
;; access methods return a list of cards in the server.
(defmulti access (fn [state side server] (get-server-type (first server))))

(defmethod access :hq [state side server]
  (concat (take (access-count state side :hq-access) (shuffle (get-in @state [:corp :hand])))
          (get-in @state [:corp :servers :hq :content])))

(defmethod access :rd [state side server]
  (concat (take (access-count state side :rd-access) (get-in @state [:corp :deck]))
          (get-in @state [:corp :servers :rd :content])))

(defmethod access :archives [state side server]
  (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
  (concat (get-in @state [:corp :discard]) (get-in @state [:corp :servers :archives :content])))

(defmethod access :remote [state side server]
  (let [contents (get-in @state [:corp :servers (first server) :content])]
    (concat contents (mapcat :hosted contents))))

(defn do-access
  "Starts the access routines for the run's server."
  [state side server]
  (trigger-event state side :pre-access (first server))
  (let [cards (access state side server)]
    ;; Cannot use `zero?` as it does not deal with `nil` nicely (throws exception)
    (when-not (or (= (get-in @state [:run :max-access]) 0)
                  (empty? cards))
      (if (= (first server) :rd)
        (let [n (count cards)]
          (system-msg state side (str "accesses " n " card" (when (> n 1) "s")))))
      (resolve-ability state side (choose-access cards server) nil nil)))
  (handle-end-run state side))

(defn replace-access
  "Replaces the standard access routine with the :replace-access effect of the card"
  [state side ability card]
  (resolve-ability state side ability card nil)
  (handle-end-run state side))

;;;; OLDER ACCESS ROUTINES. DEPRECATED.


;;; Ending runs.
(defn register-successful-run [state side server]
  (swap! state update-in [:runner :register :successful-run] #(conj % (first server)))
  (swap! state assoc-in [:run :successful] true)
  (trigger-event state side :successful-run (first server)))

(defn- successful-run-trigger
  "The real 'successful run' trigger."
  [state side]
  (when-let [successful-run-effect (get-in @state [:run :run-effect :successful-run])]
    (resolve-ability state side successful-run-effect (:card successful-run-effect) nil))
  (let [server (get-in @state [:run :server])]
    (register-successful-run state side server)
    (let [run-effect (get-in @state [:run :run-effect])
          r (:req run-effect)
          card (:card run-effect)
          replace-effect (:replace-access run-effect)]
      (if (and replace-effect
               (or (not r) (r state side card [(first server)])))
        (if (:mandatory replace-effect)
          (replace-access state side replace-effect card)
          (swap! state update-in [side :prompt]
                 (fn [p]
                   (conj (vec p) {:msg "Use Run ability instead of accessing cards?"
                                  :choices ["Run ability" "Access"]
                                  :effect #(if (= % "Run ability")
                                            (replace-access state side replace-effect card)
                                            (do-access state side server))}))))
        (do-access state side server)))))

(defn successful-run
  "Run when a run has passed all ice and the runner decides to access. The corp may still get to act in 4.3."
  [state side args]
  (if (get-in @state [:run :corp-phase-43])
    ; if corp requests phase 4.3, then we do NOT fire :successful-run yet, which does not happen until 4.4
    (do (swap! state dissoc :no-action)
        (show-wait-prompt state :runner "Corp's actions")
        (show-prompt state :corp nil "Rez and take actions before Successful Run" ["Done"]
                     (fn [args-corp]
                       (clear-wait-prompt state :runner)
                       (show-prompt state :runner nil "The run is now successful" ["Continue"]
                                    (fn [args-runner] (successful-run-trigger state :runner))))
                     {:priority -1}))
    (successful-run-trigger state side)))

(defn corp-phase-43
  "The corp indicates they want to take action after runner hits Successful Run, before access."
  [state side args]
  (swap! state assoc-in [:run :corp-phase-43] true)
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (trigger-event state side :no-action))

(defn end-run
  "End this run, and set it as UNSUCCESSFUL"
  [state side]
  (let [server (first (get-in @state [:run :server]))]
    (swap! state update-in [:runner :register :unsuccessful-run] #(conj % server))
    (swap! state assoc-in [:run :unsuccessful] true)
    (trigger-event state side :unsuccessful-run)
    (handle-end-run state side)))

(defn jack-out
  "The runner decides to jack out."
  [state side args]
  (end-run state side)
  (system-msg state side "jacks out")
  (trigger-event state side :jack-out))

(defn handle-end-run
  "Trigger appropriate events for the ending of a run."
  [state side]
  (if-not (empty? (get-in @state [:runner :prompt]))
    (swap! state assoc-in [:run :ended] true)
    (do (let [server (get-in @state [:run :server])]
          (swap! state assoc-in [:run :ending] true)
          (trigger-event state side :run-ends (first server))
          (when (get-in @state [:run :successful])
            (trigger-event state side :successful-run-ends (first server)))
          (when (get-in @state [:run :unsuccessful])
            (trigger-event state side :unsuccessful-run-ends (first server)))
          (doseq [p (filter #(has-subtype? % "Icebreaker") (all-installed state :runner))]
            (update! state side (update-in (get-card state p) [:pump] dissoc :all-run))
            (update! state side (update-in (get-card state p) [:pump] dissoc :encounter ))
            (update-breaker-strength state side p))
          (let [run-effect (get-in @state [:run :run-effect])]
            (when-let [end-run-effect (:end-run run-effect)]
              (resolve-ability state side end-run-effect (:card run-effect) [(first server)]))))
        (swap! state update-in [:runner :credit] - (get-in @state [:runner :run-credit]))
        (swap! state assoc-in [:runner :run-credit] 0)
        (swap! state assoc :run nil)
        (update-all-ice state side)
        (clear-run-register! state))))
