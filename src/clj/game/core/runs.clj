(in-ns 'game.core)

(declare any-flag-fn? clear-run-register! run-cleanup
         gain-run-credits update-ice-in-server update-all-ice
         get-agenda-points gain-agenda-point optional-ability
         get-remote-names card-name can-access-loud can-steal?
         prevent-jack-out card-flag? can-run?)

;;; Steps in the run sequence
(defn make-run
  "Starts a run on the given server, with the given card as the cause. If card is nil, assume a click was spent."
  ([state side server] (make-run state side (make-eid state) server nil nil nil))
  ([state side eid server] (make-run state side eid server nil nil nil))
  ([state side server run-effect card] (make-run state side (make-eid state) server run-effect card nil))
  ([state side eid server run-effect card] (make-run state side eid server run-effect card nil))
  ([state side eid server run-effect card {:keys [ignore-costs] :as args}]
   (wait-for (trigger-event-simult state :runner :pre-init-run nil server)
             (let [all-run-costs (when-not ignore-costs (run-costs state server card))]
               (if (and (can-run? state :runner)
                        (can-run-server? state server)
                        (can-pay? state :runner "a run" all-run-costs))
                 (do (when (= card :click-run)
                       (swap! state assoc-in [:runner :register :click-type] :run)
                       (swap! state assoc-in [:runner :register :made-click-run] true)
                       (play-sfx state side "click-run"))
                     (if-let [cost-str (pay state :runner nil all-run-costs)]
                       (do (system-msg state :runner (str (build-spend-msg cost-str "make a run on") server (when ignore-costs ", ignoring all costs")))
                           (let [s [(if (keyword? server) server (last (server->zone state server)))]
                                 ices (get-in @state (concat [:corp :servers] s [:ices]))
                                 n (count ices)]
                             ;; s is a keyword for the server, like :hq or :remote1
                             (swap! state assoc :per-run nil
                                    :run {:server s
                                          :position n
                                          :access-bonus []
                                          :run-effect (assoc run-effect :card card)
                                          :eid eid})
                             (trigger-event state side :begin-run :server s)
                             (gain-run-credits state side (get-in @state [:runner :next-run-credit]))
                             (swap! state assoc-in [:runner :next-run-credit] 0)
                             (gain-run-credits state side (+ (get-in @state [:corp :bad-publicity]) (get-in @state [:corp :has-bad-pub])))
                             (swap! state update-in [:runner :register :made-run] #(conj % (first s)))
                             (update-all-ice state :corp)
                             (swap! state update-in [:stats side :runs :started] (fnil inc 0))
                             (wait-for (trigger-event-simult state :runner :run nil s)
                                       (when (>= n 2) (trigger-event state :runner :run-big s n))
                                       (when (zero? n)
                                         (trigger-event-simult state :runner (make-eid state) :approach-server nil)))))
                       (effect-completed state side eid)))
                 (effect-completed state side eid))))))

(defn gain-run-credits
  "Add temporary credits that will disappear when the run is over."
  [state side n]
  (swap! state update-in [:runner :run-credit] (fnil + 0 0) n)
  (gain-credits state :runner n))

(defn gain-next-run-credits
  "Add temporary credits for the next run to be initiated."
  [state side n]
  (swap! state update-in [:runner :next-run-credit] (fnil + 0 0) n))

(defn access-end
  "Trigger events involving the end of the access phase, including :no-trash and :post-access-card"
  [state side eid c]
  (when-not (find-cid (:cid c) (get-in @state [:corp :discard]))
    ;; Do not trigger :no-trash if card has already been trashed
    (trigger-event state side :no-trash c))
  (when (and (is-type? c "Agenda")
             (not (find-cid (:cid c) (get-in @state [:runner :scored]))))
    (trigger-event state side :no-steal c))
  (when (and (get-card state c)
             ;; Don't increment :no-trash-or-steal if accessing a card in Archives
             (not= (:zone c) [:discard]))
    (swap! state update-in [:runner :register :no-trash-or-steal] (fnil inc 0)))
  (trigger-event-sync state side eid :post-access-card c))

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
                                       (gain-agenda-point state :runner points)
                                       (play-sfx state side "agenda-steal")
                                       (when (:run @state)
                                         (swap! state assoc-in [:run :did-steal] true))
                                       (when (card-flag? c :has-events-when-stolen true)
                                         (register-events state side (:events (card-def c)) c))
                                       (remove-old-current state side :corp))}
          :card-ability (ability-as-handler c (:stolen (card-def c)))}
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
        (costfun state side (make-eid state) card nil))
      (concat (get-in @state [:bonus :access-cost]))
      merge-costs flatten vec))

(defn access-non-agenda
  "Access a non-agenda. Show a prompt to trash for trashable cards."
  [state side eid c & {:keys [skip-trigger-event]}]
  (when-not skip-trigger-event
    (trigger-event state side :pre-trash c))
  (swap! state update-in [:stats :runner :access :cards] (fnil inc 0))
  (if (not= (:zone c) [:discard]) ; if not accessing in Archives
    ;; The card has a trash cost (Asset, Upgrade)
    (let [card (assoc c :seen true)
          card-name (:title card)
          trash-cost (trash-cost state side c)
          can-pay (when trash-cost (or (can-pay? state :runner nil :credit trash-cost)
                                       (can-pay-with-recurring? state :runner trash-cost)))]
      ;; Show the option to pay to trash the card.
      (when-not (and (is-type? card "Operation")
                     ;; Don't show the option if Edward Kim's auto-trash flag is true.
                     (card-flag? card :can-trash-operation true))
        ;; If card has already been trashed this access don't show option to pay to trash (eg. Ed Kim)
        (when-not (find-cid (:cid card) (get-in @state [:corp :discard]))
          (let [trash-ab-cards (filter #(can-trigger? state :runner (get-in (card-def %) [:interactions :trash-ability]) % [card])
                                       (concat (all-active state :runner)
                                               (get-in @state [:runner :play-area])))
                ability-strs (map #(get-in (card-def %) [:interactions :trash-ability :label]) trash-ab-cards)
                trash-cost-str (when can-pay
                                 [(str "Pay " trash-cost " [Credits] to trash")])
                ;; If the runner is forced to trash this card (Neutralize All Threats)
                forced-to-trash? (and (or can-pay
                                          (seq trash-ab-cards))
                                      (or (get-in @state [:runner :register :force-trash])
                                          (card-flag-fn? state side card :must-trash true)))
                trash-msg (when can-pay
                            (str trash-cost " [Credits] to trash " card-name " from " (name-zone :corp (:zone card))))
                pay-str (when can-pay
                          (str (if forced-to-trash? "is forced to pay " "pays ") trash-msg))
                prompt-str (str "You accessed " card-name ".")
                no-action-str (when-not forced-to-trash?
                                ["No action"])
                choices (vec (concat ability-strs trash-cost-str no-action-str))]
            (continue-ability
              state :runner
              {:async true
               :prompt prompt-str
               :choices choices
               :effect (req (cond
                              (= target "No action")
                              (access-end state side eid c)

                              (.contains target "Pay")
                              (if (> trash-cost (get-in @state [:runner :credit] 0))
                                (do (toast state side (str "You don't have the credits to pay for " card-name
                                                           ". Did you mean to first gain credits from installed cards?"))
                                    (access-non-agenda state side eid c :skip-trigger-event true))
                                (do (lose state side :credit trash-cost)
                                    (when (:run @state)
                                      (swap! state assoc-in [:run :did-trash] true)
                                      (when forced-to-trash?
                                        (swap! state assoc-in [:run :did-access] true)))
                                    (swap! state assoc-in [:runner :register :trashed-card] true)
                                    (system-msg state side pay-str)
                                    (wait-for (trash state side card nil)
                                              (access-end state side eid c))))

                              (some #(= % target) ability-strs)
                              (let [idx (.indexOf ability-strs target)
                                    trash-ab-card (nth trash-ab-cards idx)
                                    cdef (-> (card-def trash-ab-card)
                                             :interactions
                                             :trash-ability)]
                                (when (:run @state)
                                  (swap! state assoc-in [:run :did-trash] true))
                                (wait-for (resolve-ability state side cdef trash-ab-card [card])
                                          (access-end state side eid c)))))}
              card nil)))))
    (access-end state side eid c)))

(defn- steal-pay-choice
  "Enables a vector of costs to be resolved in the order of choosing"
  [state side cost-map {:keys [title cid] :as card}]
  {:async true
   :prompt (str "Pay steal cost for " title "?")
   :choices (conj (vec (keys cost-map)) "No action")
   :effect (req
             (if (= target "No action")
               (continue-ability state :runner
                                 {:async true
                                  :effect (req (when-not (find-cid cid (:deck corp))
                                                 (system-msg state side (str "decides not to pay to steal " title)))
                                               (access-end state side eid card))}
                 card nil)
               (let [cost (cost-map target)
                     kw (first cost)
                     v (second cost)]
                 (if (and cost (can-pay? state side title [kw v]))
                   (wait-for
                     (pay-sync state side nil [kw v] {:action :steal-cost})
                     (do (system-msg state side (str "pays " target " to steal " title))
                         (if (> (count cost-map) 1)
                           (continue-ability
                             state side
                             (steal-pay-choice state :runner (dissoc cost-map target) card)
                             card nil)
                           (steal-agenda state side eid card))))
                   (access-end state side eid card)))))})

(defn- access-agenda
  "Rules interactions for a runner that has accessed an agenda and may be able to steal it."
  [state side eid c]
  (trigger-event state side :pre-steal-cost c)
  (swap! state update-in [:stats :runner :access :cards] (fnil inc 0))
  (let [cost (steal-cost state side c)
        part-cost (partition 2 cost)
        cost-strs (map costs->symbol part-cost)
        cost-map (zipmap cost-strs part-cost)
        n (count cost-strs)
        card-name (:title c)
        can-pay-costs? (can-pay? state side card-name cost)
        cost-as-symbol (when (= 1 (count cost-strs)) (costs->symbol cost))
        ;; any trash abilities
        can-steal-this? (can-steal? state side c)
        trash-ab-cards (when (not= (:zone c) [:discard])
                         (filter #(can-trigger? state :runner (get-in (card-def %) [:interactions :trash-ability]) % [c])
                                 (concat (all-active state :runner)
                                         (get-in @state [:runner :play-area]))))
        ability-strs (map #(get-in (card-def %) [:interactions :trash-ability :label]) trash-ab-cards)
        ;; strs
        steal-str (when (and can-steal-this? can-pay-costs?)
                    (if (seq cost-strs)
                      (if (= n 1)
                        [(str "Pay " cost-as-symbol " to steal")]
                        ["Pay to steal"])
                      ["Steal"]))
        no-action-str (when (or (nil? steal-str)
                                (not= steal-str ["Steal"]))
                        ["No action"])
        prompt-str (str "You accessed " card-name ".")
        choices (vec (concat ability-strs steal-str no-action-str))]
    ;; Steal costs are additional costs and can be denied by the runner.
    (continue-ability state :runner
                      {:async true
                       :prompt prompt-str
                       :choices choices
                       :effect (req (cond
                                      ;; Can't steal or pay, or won't pay single additional cost to steal
                                      (= target "No action")
                                      (access-end state side eid c)

                                      ;; Steal normally
                                      (= target "Steal")
                                      (steal-agenda state :runner eid c)

                                      ;; Pay single additiional cost to steal
                                      (.contains target "Pay")
                                      (if (> n 1)
                                        ;; Use the better function for multiple costs
                                        (continue-ability state :runner
                                                          (steal-pay-choice state :runner cost-map c)
                                                          c nil)
                                        ;; Otherwise, just handle everything right friggin here
                                        (wait-for (pay-sync state side nil cost {:action :steal-cost})
                                                  (do (system-msg state side
                                                                  (str "pays " cost-as-symbol " to steal " card-name))
                                                      (steal-agenda state side eid c))))

                                      ;; Use trash ability
                                      (some #(= % target) ability-strs)
                                      (let [idx (.indexOf ability-strs target)
                                            trash-ab-card (nth trash-ab-cards idx)
                                            cdef (-> (card-def trash-ab-card)
                                                     :interactions
                                                     :trash-ability)]
                                        (when (:run @state)
                                          (swap! state assoc-in [:run :did-trash] true))
                                        (wait-for (resolve-ability state side cdef trash-ab-card [c])
                                                  (do (trigger-event state side :no-steal c)
                                                      (access-end state side eid c))))))}
                      c nil)))

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

(defn msg-handle-access
  "Generate the message from the access"
  [state side {:keys [zone] :as card} title]
  (let [msg (str "accesses " title
                 (when card
                   (str " from " (name-zone side zone))))]
    (system-msg state side msg)
    (when (reveal-access? state side card)
      (system-msg state side (str "must reveal they accessed " (:title card)))
      (reveal state :runner card))))

(defn- access-trigger-events
  "Trigger access effects, then move into trash/steal choice."
  [state side eid c title]
  (let [cdef (card-def c)
        c (assoc c :seen true)
        access-effect (when-let [acc (:access cdef)]
                        (ability-as-handler c acc))]
    (msg-handle-access state side c title)
    (wait-for (trigger-event-simult state side :access
                                    {:card-ability access-effect
                                     ;; Cancel other access handlers if the card moves zones because of a handler
                                     :cancel-fn (fn [state] (not (get-card state c)))}
                                    c)
              (if (get-card state c) ; make sure the card has not been moved by a handler
                (if (is-type? c "Agenda")
                  (access-agenda state side eid c)
                  ;; Accessing a non-agenda
                  (access-non-agenda state side eid c))
                (access-end state side eid c)))))

(defn- access-pay
  "Force the runner to pay any costs to access this card, if any, before proceeding with access."
  [state side eid c title]
  (let [acost (access-cost state side c)
        ;; hack to prevent toasts when playing against Gagarin and accessing on 0 credits
        anon-card (dissoc c :title)
        cant-pay {:prompt "You can't pay the cost to access this card"
                  :choices ["OK"]
                  :async true
                  :effect (effect (access-end eid c))}]
    (cond
      ;; Check if a pre-access-card effect trashed the card (By Any Means)
      (not (get-card state c))
      (access-end state side eid c)

      ;; Either there were no access costs, or the runner could pay them.
      (empty? acost)
      (access-trigger-events state side eid c title)

      (not-empty acost)
      ;; Await the payment of the costs; if payment succeeded, proceed with access.
      (wait-for (pay-sync state side anon-card acost)
                (if async-result
                  (access-trigger-events state side eid c title)
                  (resolve-ability state :runner eid cant-pay c nil)))
      :else
      ;; The runner cannot afford the cost to access the card
      (resolve-ability state :runner eid cant-pay c nil))))

(defn access-card
  "Apply game rules for accessing the given card."
  ([state side card] (access-card state side (make-eid state) card nil))
  ([state side eid card] (access-card state side eid card (:title card)))
  ([state side eid card title]
    ;; Indicate that we are in the access step.
   (swap! state assoc :access true)
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
   :choices {:req #(some (fn [c] (= (:cid %) (:cid c))) cards)}
   :async true
   :effect (req (wait-for (access-card state side target)
                          (if (< 1 (count cards))
                            (continue-ability state side (access-helper-remote (filter #(not= (:cid %) (:cid target)) cards))
                                              card nil)
                            (effect-completed state side eid))))})

(defmethod choose-access :remote [cards server args]
  {:async true
   :effect (req (if (and (>= 1 (count cards))
                         (not (any-flag-fn? state :runner :slow-remote-access true
                                            (concat (all-active state :runner) (all-active state :corp)))))
                  (access-card state side eid (first cards))
                  (continue-ability state side (access-helper-remote cards) card nil)))})

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
                           (filter #(not (contains? already-accessed %)) (get-in @state [:corp :servers chosen-zone :content])))
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
                          unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %)))
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
                           :choices {:req #(and (= (second (:zone %)) chosen-zone)
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

(defmethod choose-access :rd [cards server {:keys [no-root] :as args}]
  {:async true
   :effect (req (if (pos? (count cards))
                  (if (= 1 (count cards))
                    (access-card state side eid (first cards) "an unseen card")
                    (let [from-rd (access-count state side :rd-access)]
                      (continue-ability state side (access-helper-hq-or-rd
                                                     state :rd "deck" from-rd
                                                     ;; access the first card in deck that has not been accessed.
                                                     (fn [already-accessed] (first (drop-while already-accessed
                                                                                               (-> @state :corp :deck))))
                                                     (fn [_] "an unseen card")
                                                     (if no-root
                                                       (set (get-in @state [:corp :servers :rd :content]))
                                                       #{}))
                                        card nil)))
                  (effect-completed state side eid)))})

(defmethod choose-access :hq [cards server {:keys [no-root] :as args}]
  {:async true
   :effect (req (if (pos? (count cards))
                  (if (and (= 1 (count cards))
                           (not (any-flag-fn? state :runner :slow-hq-access true)))
                    (access-card state side eid (first cards))
                    (let [from-hq (min (access-count state side :hq-access)
                                       (-> @state :corp :hand count))
                          ; Handle root only access - no cards to access in hand
                          from-hq (if (some #(= '[:hand] (:zone %)) cards) from-hq 0)]
                      (continue-ability state side (access-helper-hq-or-rd
                                                     state :hq "hand" from-hq
                                                     (fn [already-accessed] (some #(when-not (already-accessed %) %)
                                                                                  (shuffle (-> @state :corp :hand))))
                                                     (fn [card] (:title card))
                                                     (if no-root
                                                       (set (get-in @state [:corp :servers :hq :content]))
                                                       #{}))
                                        card nil)))
                  (effect-completed state side eid)))})


(defn access-helper-hq
  "This is a helper for cards to invoke HQ access without knowing how to use the full access method. See Dedicated Neural Net."
  [state from-hq already-accessed]
  (access-helper-hq-or-rd state :hq "hand" from-hq
                          (fn [already-accessed] (some #(when-not (already-accessed %) %)
                                                       (shuffle (-> @state :corp :hand))))
                          :title
                          already-accessed))


(defn- get-archives-accessible [state]
  ;; only include agendas and cards with an :access ability whose :req is true
  ;; (or don't have a :req, or have an :optional with no :req, or :optional with a true :req.)
  (filter #(let [cdef (card-def %)]
             ;; must also be :seen
             (and (:seen %)
                  (or (is-type? % "Agenda")
                      (should-trigger? state :corp % nil (:access cdef)))))
          (get-in @state [:corp :discard])))

(defn- get-archives-inactive [state]
  ;; get faceup cards with no access interaction
  (filter #(let [cdef (card-def %)]
             (and (:seen %)
                  (not (or (is-type? % "Agenda")
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
                    (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %)))
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
                           :choices {:req #(and (= (second (:zone %)) :archives)
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
   :effect (req (let [cards (concat (get-archives-accessible state) (-> @state :corp :servers :archives :content))
                      archives-count (+ (count (-> @state :corp :discard)) (count (-> @state :corp :servers :archives :content)))]
                  ;; Because we don't "access" cards in Archives like normal,
                  ;; we have to manually count all the cards we'd normally skip
                  (swap! state update-in [:run :cards-accessed :discard] (fnil + 0 0) (- archives-count (count cards)))
                  (if (not-empty cards)
                    (if (= 1 archives-count)
                      (access-card state side eid (first cards))
                      (continue-ability state side
                                        (access-helper-archives state archives-count
                                                                (if no-root
                                                                  (set (get-in @state [:corp :servers :archives :content]))
                                                                  #{}))
                                        card nil))
                    (effect-completed state side eid))))})

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
  (concat (take (access-count state side :rd-access) (get-in @state [:corp :deck]))
          (get-in @state [:corp :servers :rd :content])))

(defmethod cards-to-access :archives [state side server]
  (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
  (concat (get-in @state [:corp :discard]) (get-in @state [:corp :servers :archives :content])))

(defmethod cards-to-access :remote [state side server]
  (let [contents (get-in @state [:corp :servers (first server) :content])]
    (filter (partial can-access-loud state side) (concat contents (get-all-hosted contents)))))

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
                   n (count cards)]
               ;; Make `:did-access` true when reaching the access step (no replacement)
               (when (:run @state)
                 (swap! state assoc-in [:run :did-access] true))
               (if (or (zero? n)
                       (safe-zero? (get-in @state [:run :max-access])))
                 (do (system-msg state side "accessed no cards during the run")
                     (effect-completed state side eid))
                 (do (swap! state assoc-in [:runner :register :accessed-cards] true)
                     (wait-for (resolve-ability state side (choose-access cards server args) nil nil)
                               (wait-for (trigger-event-sync state side :end-access-phase {:from-server (first server)})
                                         (effect-completed state side eid)))))))))

(defn replace-access
  "Replaces the standard access routine with the :replace-access effect of the card"
  [state side ability card]
  (wait-for (resolve-ability state side ability card nil)
            (run-cleanup state side)))

;;;; OLDER ACCESS ROUTINES. DEPRECATED.


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

(defn- successful-run-trigger
  "The real 'successful run' trigger."
  [state side]
  (let [successful-run-effect (get-in @state [:run :run-effect :successful-run])
        card (get-in @state [:run :run-effect :card])]
    (when (and successful-run-effect
               (not (apply trigger-suppress state side :successful-run card)))
      (resolve-ability state side successful-run-effect (:card successful-run-effect) nil)))
  (wait-for (register-successful-run state side (get-in @state [:run :server]))
            (let [the-run (:run @state)
                  server (:server the-run) ; bind here as the server might have changed
                  run-effect (:run-effect the-run)
                  run-req (:req run-effect)
                  card (:card run-effect)
                  replace-effect (:replace-access run-effect)]
              (if (:ended (:run @state))
                (run-cleanup state side)
                (if (:prevent-access the-run)
                  (do (system-msg state :runner "is prevented from accessing any cards this run")
                      (resolve-ability state :runner
                                       {:prompt "You are prevented from accessing any cards this run."
                                        :choices ["OK"]
                                        :effect (effect (handle-end-run))}
                                       nil nil))
                  (if (and replace-effect
                           (or (not run-req)
                               (run-req state side (make-eid state) card [(first server)])))
                    (if (:mandatory replace-effect)
                      (replace-access state side replace-effect card)
                      (swap! state update-in [side :prompt]
                             (fn [p]
                               (conj (vec p) {:msg "Use replacement effect instead of accessing cards?"
                                              :choices ["Replacement effect" "Access cards"]
                                              :effect #(if (= % "Replacement effect")
                                                         (replace-access state side replace-effect card)
                                                         (wait-for (do-access state side server)
                                                                   (handle-end-run state side)))}))))
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
   (let [run (:run @state)
         server (first (get-in @state [:run :server]))]
     (swap! state update-in [:runner :register :unsuccessful-run] #(conj % server))
     (swap! state assoc-in [:run :unsuccessful] true)
     (handle-end-run state side)
     (trigger-event-sync state side eid :unsuccessful-run run))))

(defn end-run
  "After checking for prevents, end this run, and set it as UNSUCCESSFUL."
  ([state side] (end-run state side (make-eid state)))
  ([state side eid] (end-run state side eid nil))
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
  (end-run state side)
  (system-msg state side "jacks out")
  (trigger-event-sync state side (make-result eid true) :jack-out))

(defn jack-out
  "The runner decides to jack out."
  ([state side] (jack-out state side (make-eid state)))
  ([state side eid]
   (swap! state update-in [:jack-out] dissoc :jack-out-prevent)
   (wait-for (trigger-event-sync state side :pre-jack-out)
             (let [prevent (get-prevent-list state :corp :jack-out)]
               (if (cards-can-prevent? state :corp prevent :jack-out)
                 (do (system-msg state :corp "has the option to prevent the Runner from jacking out")
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
                 (do (resolve-jack-out state side eid)
                     (effect-completed state side (make-result eid false))))))))

(defn- trigger-run-end-events
  [state side eid run]
  (cond
    ;; Successful
    (:successful run)
    (do
      (play-sfx state side "run-successful")
      (wait-for (trigger-event-simult state side :successful-run-ends nil run)
                (effect-completed state side (make-result eid {:successful true}))))
    ;; Unsuccessful
    (:unsuccessful run)
    (do
      (play-sfx state side "run-unsuccessful")
       (wait-for (trigger-event-sync state side :unsuccessful-run-ends run)
                (effect-completed state side (make-result eid {:unsuccessful true}))))

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
    (update-all-ice state side)
    (swap! state dissoc :access)
    (clear-run-register! state)
    (trigger-run-end-events state side (:eid run) run)))

(defn run-cleanup
  "Trigger appropriate events for the ending of a run."
  [state side]
  (let [server (-> @state :run :server first)]
    (swap! state assoc-in [:run :ending] true)
    (trigger-event state side :run-ends server)
    (doseq [p (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))]
      (update! state side (update-in (get-card state p) [:pump] dissoc :all-run))
      (update! state side (update-in (get-card state p) [:pump] dissoc :encounter))
      (update-breaker-strength state side p))
    (let [run-effect (get-in @state [:run :run-effect])]
      (if-let [end-run-effect (:end-run run-effect)]
        (wait-for (resolve-ability state side end-run-effect (:card run-effect) [server])
                  (run-cleanup-2 state side))
        (run-cleanup-2 state side)))))

(defn handle-end-run
  "Initiate run resolution."
  [state side]
  (if-not (and (empty? (get-in @state [:runner :prompt]))
               (empty? (get-in @state [:corp :prompt])))
    (swap! state assoc-in [:run :ended] true)
    (run-cleanup state side)))

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
