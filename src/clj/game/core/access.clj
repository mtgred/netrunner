(in-ns 'game.core)

(defn no-trash-or-steal
  [state]
  (swap! state update-in [:runner :register :no-trash-or-steal] (fnil inc 0)))

(defn access-end
  "Trigger events involving the end of the access phase, including :no-trash and :post-access-card"
  ([state side eid c] (access-end state side eid c nil))
  ([state side eid c {:keys [trashed stolen] :as args}]
   ;; Do not trigger :no-trash if card has already been trashed
   (wait-for (trigger-event-sync state side (when-not trashed :no-trash) c)
             (wait-for (trigger-event-sync state side (when-not stolen :no-steal) c)
                       (when (and (not trashed)
                                  (not stolen)
                                  ;; Don't increment :no-trash-or-steal if accessing a card in Archives
                                  (not= (:zone c) [:discard]))
                         (no-trash-or-steal state))
                       (swap! state dissoc :access)
                       (trigger-event-sync state side eid :post-access-card c)))))

;;; Accessing rules
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
                            (seq (filter #(can-trigger? state :runner eid (access-ab %) % [card])
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
                                            (access-end state side eid (first async-result) {:trashed true})))

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
                                    (let [card (first async-result)]
                                      (access-end state side eid card {:trashed (in-discard? card)}))))))}
        card nil))))

;;; Stealing agendas
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

(defn steal
  "Moves a card to the runner's :scored area, triggering events from the completion of the steal."
  [state side eid card]
  (let [c (move state :runner (dissoc card :advance-counter :new) :scored {:force true})
        points (get-agenda-points state :runner c)]
    (wait-for
      (trigger-event-simult
        state :runner :agenda-stolen
        {:first-ability {:async true
                         :effect (req (system-msg state :runner (str "steals " (:title c) " and gains "
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
                                      (remove-old-current state side eid :corp))}
         :card-abilities (ability-as-handler c (:stolen (card-def c)))}
        c)
      (access-end state side eid c {:stolen true}))))

(defn- steal-agenda
  "Trigger the stealing of an agenda, now that costs have been paid."
  [state side eid card]
  (let [cdef (card-def card)]
    (if (or (not (:steal-req cdef))
            ((:steal-req cdef) state :runner eid card nil))
      (steal state :runner eid card)
      (access-end state side eid card))))

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
                          (seq (filter #(can-trigger? state :runner eid (access-ab %) % [card])
                                       (all-active state :runner))))
        ability-strs (mapv access-ab-label access-ab-cards)
        ;; strs
        steal-str (when (and can-steal can-pay)
                    (if (not (blank? cost-strs))
                      ["Pay to steal"]
                      ["Steal"]))
        no-action-str (when-not (= steal-str ["Steal"])
                        ["No action"])
        prompt-str (if (not (blank? cost-strs))
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
                                  (let [card (first async-result)]
                                    (trigger-event state side :no-steal card)
                                    (access-end state side eid card {:stolen (in-scored? card)}))))))}
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
  [state side {:keys [zone] :as card} title {:keys [cost-msg]}]
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
  [state side eid c title {:keys [no-msg] :as args}]
  (let [cdef (card-def c)
        c (assoc c :seen true)
        access-effect (when-let [acc (:access cdef)]
                        (ability-as-handler c acc))]
    (swap! state assoc-in [:runner :register :accessed-cards] true)
    (when-not no-msg
      (msg-handle-access state side c title args))
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
                (access-end state side eid c {:trashed (find-cid (:cid c) (get-in @state [:corp :discard]))
                                              :stolen (and (agenda? c)
                                                           (find-cid (:cid c) (get-in @state [:runner :scored])))})))))

(defn access-cost-bonus
  "Applies a cost to the next access. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :access-cost] #(merge-costs (concat % costs))))

(defn access-cost
  "Gets a vector of costs for accessing the given card."
  [state side]
  (merge-costs (get-in @state [:bonus :access-cost])))

(defn- access-pay
  "Force the runner to pay any costs to access this card, if any, before proceeding with access."
  [state side eid card title args]
  (let [cost (access-cost state side)
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
        (let [accessed-card card]
          {:async true
           :prompt prompt-str
           :choices choices
           :effect (req (if (or (= "OK" target)
                                (= "No action" target))
                          (access-end state side eid accessed-card)
                          (wait-for (pay-sync state side accessed-card cost)
                                    (if async-result
                                      (access-trigger-events state side eid accessed-card title (assoc args :cost-msg async-result))
                                      (access-end state side eid accessed-card)))))})
        nil nil)
      ;; There are no access costs
      :else
      (access-trigger-events state side eid card title args))))

(defn access-card
  "Apply game rules for accessing the given card."
  ([state side eid card] (access-card state side eid card (:title card) nil))
  ([state side eid card title] (access-card state side eid card title nil))
  ([state side eid card title args]
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
             (access-pay state side eid card title args))))



(defn set-only-card-to-access
  [state side card]
  (swap! state assoc-in [:run :only-card-to-access] card))

(defn get-only-card-to-access
  [state]
  (get-card state (get-in @state [:run :only-card-to-access])))

(defn get-all-hosted [hosts]
  (let [hosted-cards (mapcat :hosted hosts)]
    (if (empty? hosted-cards)
      hosted-cards
      (concat hosted-cards (get-all-hosted hosted-cards)))))

(defn get-all-content [content]
  (remove :condition (concat content (get-all-hosted content))))

;;; Methods for allowing user-controlled multi-access in servers.
(defmulti must-continue?
  (fn [state already-accessed-fn amount-access args]
    (get-server-type (first (:server args)))))

(defmethod must-continue? :remote
  [state already-accessed-fn access-amount args]
  (and (pos? (:total access-amount))
       (pos? (->> (get-all-content (get-in @state [:corp :servers (first (:server args)) :content]))
                  (remove already-accessed-fn)
                  count))))

(defn access-helper-remote
  [state {:keys [base total] :as access-amount} already-accessed {:keys [no-root server] :as args}]
  (let [current-available (->> (get-in @state [:corp :servers (first server) :content])
                               get-all-content
                               (map :cid)
                               set)
        already-accessed (clj-set/intersection already-accessed current-available)
        available (clj-set/difference current-available already-accessed)
        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))]
    (when (must-continue? state already-accessed-fn access-amount args)
      {:prompt "Click a card to access it. You must access all cards in this server."
       :choices {:card #(contains? available (:cid %))}
       :async true
       :effect (req (wait-for (access-card state side target)
                              (continue-ability
                                state side
                                (access-helper-remote
                                  state {:base (dec base) :total (dec total)}
                                  (conj already-accessed (:cid target))
                                  args)
                                card nil)))})))

(defmulti choose-access
  ;; choose-access implements game prompts allowing the runner to choose the order of access
  (fn [access-amount server args]
    (get-server-type (first server))))

(defmethod choose-access :remote
  [{:keys [base total] :as access-amount} server args]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      content (get-in @state [:corp :servers (first server) :content])
                      total-cards (or (when only-card [only-card])
                                      (get-all-content content))
                      total-cards-count (count total-cards)
                      pos-total? (pos? total)
                      pos-total-cards? (pos? total-cards-count)]

                  (cond
                    ;; Only 1 card to access
                    (and pos-total?
                         (= 1 total-cards-count))
                    (access-card state side eid (first total-cards))

                    ;; Normal access
                    (and pos-total?
                         pos-total-cards?)
                    (continue-ability
                      state side
                      (access-helper-remote state access-amount #{} args)
                      card nil)

                    :else
                    (effect-completed state side eid))))})

(defn- access-cards-from-rd
  [state]
  (let [f (get-in @state [:runner :rd-access-fn])]
    (f (get-in @state [:corp :deck]))))

(defn root-content
  [state server already-accessed-fn]
  (remove already-accessed-fn (get-in @state [:corp :servers server :content])))

(defmethod must-continue? :rd
  [state already-accessed-fn access-amount {:keys [no-root idx] :as args}]
  (and (pos? (:total access-amount))
       (pos? (count (concat (let [deck (access-cards-from-rd state)
                                  card-to-see (nth deck idx nil)]
                              (when card-to-see
                                [card-to-see]))
                            (when-not no-root
                              (root-content state :rd already-accessed-fn)))))))

(defn access-helper-rd
  [state {:keys [base total] :as access-amount} already-accessed {:keys [no-root idx] :as args}]
  (let [
        ;; already-accessed is only used for upgrades
        current-available (set (map :cid (get-in @state [:corp :servers :rd :content])))
        already-accessed (clj-set/intersection already-accessed current-available)
        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))

        deck (access-cards-from-rd state)
        card-to-access (nth deck idx nil)

        card-from "Card from deck"
        card-from-button (when (and (pos? base)
                                    card-to-access)
                           [card-from])
        root (root-content state :rd already-accessed-fn)
        upgrade-buttons (when-not no-root
                          (->> root
                               (filter rezzed?)
                               (map :title)))
        unrezzed-card "Unrezzed upgrade"
        unrezzed-cards-button (when (and (not no-root)
                                         (seq (filter (complement rezzed?) root)))
                                [unrezzed-card])
        choices (concat card-from-button
                        upgrade-buttons
                        unrezzed-cards-button)

        card-from-deck-fn
        (req
          (wait-for (access-card state side card-to-access "an unseen card")
                    (let [shuffled-during-run (get-in @state [:run :shuffled-during-access :rd])
                          idx (if (get-card state card-to-access)
                                (inc idx)
                                idx)
                          ;; if R&D was shuffled because of the access,
                          ;; the runner "starts over" from the top
                          args (if shuffled-during-run
                                 (assoc args :idx 0)
                                 (assoc args :idx idx))]
                      (if shuffled-during-run
                        (swap! state update-in [:run :shuffled-during-access] dissoc :rd))
                      (continue-ability
                        state side
                        (access-helper-rd
                          state {:base (dec base)
                                 :total (dec total)}
                          already-accessed
                          args)
                        nil nil))))

        unrezzed-cards-fn
        (req
          (let [unrezzed (filter (complement rezzed?) root)]
            (if (= 1 (count unrezzed))
              ;; only one unrezzed upgrade; access it and continue
              (wait-for (access-card state side (first unrezzed))
                        (continue-ability
                          state side
                          (access-helper-rd
                            state {:base base :total (dec total)}
                            (conj already-accessed (:cid (first unrezzed))) args)
                          nil nil))
              ;; more than one unrezzed upgrade. allow user to select with mouse.
              (continue-ability
                state side
                {:async true
                 :prompt "Choose an upgrade in root of R&D to access."
                 :choices {:card (fn [card] (some #(same-card? card %) unrezzed))}
                 :effect (req (wait-for (access-card state side target)
                                        (continue-ability
                                          state side
                                          (access-helper-rd
                                            state {:base base :total (dec total)}
                                            (conj already-accessed (:cid target)) args)
                                          nil nil)))}
                nil nil))))]
    (when (and (seq choices) (must-continue? state already-accessed-fn access-amount args))
      (cond

        (= choices card-from-button)
        {:async true
         :effect card-from-deck-fn}

        (= choices unrezzed-cards-button)
        {:async true
         :effect unrezzed-cards-fn}

        :else
        {:async true
         :prompt "Select a card to access."
         :choices choices
         :effect (req (cond

                        ;; accessing a card in deck
                        (= target card-from)
                        (card-from-deck-fn state side eid nil nil)

                        ;; accessing an unrezzed upgrade
                        (= target unrezzed-card)
                        (unrezzed-cards-fn state side eid nil nil)

                        ;; accessing a rezzed upgrade
                        :else
                        (let [accessed (some #(when (= (:title %) target) %) root)]
                          (wait-for (access-card state side accessed)
                                    (continue-ability
                                      state side
                                      (access-helper-rd
                                        state {:base base :total (dec total)}
                                        (conj already-accessed (:cid accessed)) args)
                                      card nil)))))}))))

(defmethod choose-access :rd
  [{:keys [base total] :as access-amount} server {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      total-cards (or (when only-card [only-card])
                                      (concat
                                        (take base (access-cards-from-rd state))
                                        (when-not no-root
                                          (-> @state :corp :servers :rd :content))))
                      total-cards-count (count total-cards)
                      pos-total? (pos? total)
                      pos-total-cards? (pos? total-cards-count)
                      args (assoc args :idx 0)]

                  (cond
                    ;; Only 1 card to access
                    (and pos-total?
                         (= 1 total-cards-count)
                         only-card)
                    (access-card state side eid (first total-cards))

                    ;; Normal access
                    (and pos-total?
                         pos-total-cards?)
                    (continue-ability
                      state side
                      (access-helper-rd state access-amount #{} args)
                      card nil)

                    :else
                    (effect-completed state side eid))))})

(defmethod must-continue? :hq
  [state already-accessed-fn access-amount {:keys [no-root] :as args}]
  (and (pos? (:total access-amount))
       (pos? (->> (concat (get-in @state [:corp :hand])
                          (when-not no-root
                            (get-in @state [:corp :servers :hq :content])))
                  (remove already-accessed-fn)
                  count))))

(defn- access-cards-from-hq
  [state]
  (let [f (get-in @state [:runner :hq-access-fn])]
    (f (get-in @state [:corp :hand]))))

(defn access-helper-hq
  [state {:keys [base total] :as access-amount}
   already-accessed {:keys [no-root access-first] :as args}]
  (let [
        hand (get-in @state [:corp :hand])
        current-available (set (concat (map :cid hand)
                                       (map :cid (get-in @state [:corp :servers :hq :content]))))
        already-accessed (clj-set/intersection already-accessed current-available)

        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))

        card-from "Card from hand"
        card-from-button (when (and (pos? base)
                                    (seq (remove already-accessed-fn hand)))
                           [card-from])
        root (root-content state :hq already-accessed-fn)
        upgrade-buttons (when-not no-root
                          (->> root
                               (filter rezzed?)
                               (map :title)))
        unrezzed-card "Unrezzed upgrade"
        unrezzed-cards-button (when (and (not no-root)
                                         (->> root
                                              (filter (complement rezzed?))
                                              (remove already-accessed-fn)
                                              seq))
                                [unrezzed-card])
        choices (concat card-from-button
                        upgrade-buttons
                        unrezzed-cards-button)

        card-from-hand-fn
        (req
          (let [accessed (first (drop-while already-accessed-fn (access-cards-from-hq state)))]
            (wait-for (access-card state side accessed (:title accessed))
                      (continue-ability
                        state side
                        (access-helper-hq
                          state {:base (dec base) :total (dec total)}
                          (conj already-accessed (:cid accessed)) args)
                        card nil))))

        unrezzed-cards-fn
        (req
          (let [unrezzed (filter (complement rezzed?) root)]
            (if (= 1 (count unrezzed))
              ;; only one unrezzed upgrade; access it and continue
              (wait-for (access-card state side (first unrezzed))
                        (continue-ability
                          state side
                          (access-helper-hq
                            state {:base base :total (dec total)}
                            (conj already-accessed (:cid (first unrezzed))) args)
                          nil nil))
              ;; more than one unrezzed upgrade. allow user to select with mouse.
              (continue-ability
                state side
                {:async true
                 :prompt "Choose an upgrade in root of HQ to access."
                 :choices {:card (fn [card] (some #(same-card? card %) unrezzed))}
                 :effect (req (wait-for (access-card state side target)
                                        (continue-ability
                                          state side
                                          (access-helper-hq
                                            state {:base base :total (dec total)}
                                            (conj already-accessed (:cid target)) args)
                                          nil nil)))}
                nil nil))))]

    (when (and (seq choices) (must-continue? state already-accessed-fn access-amount args))
      (cond
        (seq access-first)
        {:async true
         :effect (req (let [accessed (first access-first)]
                        (wait-for (access-card state side accessed (:title accessed))
                                  (continue-ability
                                    state side
                                    (access-helper-hq
                                      state {:base (dec base) :total (dec total)}
                                      (conj already-accessed (:cid accessed))
                                      (assoc args :access-first (next access-first)))
                                    nil nil))))}

        (= choices card-from-button)
        {:async true
         :effect card-from-hand-fn}

        (= choices unrezzed-cards-button)
        {:async true
         :effect unrezzed-cards-fn}

        :else
        {:async true
         :prompt "Select a card to access."
         :choices choices
         :effect (req (cond

                        ;; accessing a card in hand
                        (= target card-from)
                        (card-from-hand-fn state side eid nil nil)

                        ;; accessing an unrezzed upgrade
                        (= target unrezzed-card)
                        (unrezzed-cards-fn state side eid nil nil)

                        ;; accessing a rezzed upgrade
                        :else
                        (let [accessed (some #(when (= (:title %) target) %) root)]
                          (wait-for (access-card state side accessed)
                                    (continue-ability
                                      state side
                                      (access-helper-hq
                                        state {:base base :total (dec total)}
                                        (conj already-accessed (:cid accessed)) args)
                                      card nil)))))}))))

(defmethod choose-access :hq
  [{:keys [base total] :as access-amount} server {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      total-cards (or (when only-card [only-card])
                                      (concat
                                        (get-in @state [:corp :hand])
                                        (when-not no-root
                                          (get-in @state [:corp :servers :hq :content]))))
                      total-cards-count (count total-cards)
                      pos-total? (pos? total)
                      pos-total-cards-count? (pos? total-cards-count)]

                  (cond
                    ;; Only 1 card to access
                    (and pos-total?
                         (= 1 total-cards-count))
                    (access-card state side eid (first total-cards))

                    ;; Corp chooses accessed cards
                    (and pos-total?
                         pos-total-cards-count?
                         (any-effects state side :corp-choose-hq-access))
                    (do (show-wait-prompt state :runner "Corp to select cards in HQ to be accessed")
                        (continue-ability
                          state :corp
                          {:prompt (str "Select " (min base (-> @state :corp :hand count))
                                        " cards in HQ for the Runner to access")
                           :choices {:card #(and (in-hand? %)
                                                 (corp? %))
                                     :all true
                                     :max (req (min base (-> @state :corp :hand count)))}
                           :async true
                           :effect (req (clear-wait-prompt state :runner)
                                        (continue-ability
                                          state :runner
                                          (access-helper-hq
                                            state access-amount
                                            ; access-helper-hq uses a set to keep track of which cards have already
                                            ; been accessed. Using the set difference we make the runner unable to
                                            ; access non-selected cards from the corp prompt
                                            (clj-set/difference (set (map :cid (:hand corp)))
                                                                (set (map :cid targets)))
                                            args)
                                          card nil))}
                          card nil))

                    ;; Normal access
                    (and pos-total?
                         pos-total-cards-count?)
                    (continue-ability
                      state side
                      (access-helper-hq state access-amount #{} args)
                      card nil)

                    ;; No cards to access
                    :else
                    (effect-completed state side eid))))})

(defn- accessible? [state card]
  (or (agenda? card)
      (should-trigger? state :corp (make-eid state) card nil (:access (card-def card)))))

(defn- get-archives-accessible [state]
  ;; only include agendas and cards with an :access ability that can trigger
  (filter #(and (:seen %) (accessible? state %)) (get-in @state [:corp :discard])))

(defn- get-archives-inactive [state]
  ;; get faceup cards with no access interaction
  (filter #(and (:seen %) (not (accessible? state %))) (get-in @state [:corp :discard])))

(defn- access-inactive-archives-cards
  ([state side eid cards access-amount] (access-inactive-archives-cards state side eid cards access-amount '()))
  ([state side eid cards {:keys [base total] :as access-amount} accessed-cards]
   (if (pos? (:total access-amount))
     (wait-for (access-card state side (first cards) nil {:no-msg true})
               (let [access-amount {:base (dec base)
                                    :total (dec total)}]
                 (access-inactive-archives-cards state side eid (next cards) access-amount (cons (first cards) accessed-cards))))
     (complete-with-result state side eid accessed-cards))))

(defn faceup-accessible
  [state already-accessed-fn]
  (remove already-accessed-fn
          (or (when-let [only-card (get-only-card-to-access state)]
                [only-card])
              (get-archives-accessible state))))

(defn facedown-cards
  [state already-accessed-fn]
  (filter #(and (not (:seen %))
                (not (already-accessed-fn %)))
          (or (when-let [only-card (get-only-card-to-access state)]
                        [only-card])
              (get-in @state [:corp :discard]))))

(defmethod must-continue? :archives
  [state already-accessed-fn access-amount {:keys [no-root] :as args}]
  (and (pos? (:total access-amount))
       (pos? (->> (concat (get-in @state [:corp :discard])
                          (when-not no-root
                            (get-in @state [:corp :servers :archives :content])))
                  (remove already-accessed-fn)
                  count))))

(defn access-helper-archives
  [state {:keys [base total] :as access-amount} already-accessed {:keys [no-root] :as args}]
  (let [
        current-available (set (concat (map :cid (get-in @state [:corp :discard]))
                                       (map :cid (get-in @state [:corp :servers :archives :content]))))
        already-accessed (clj-set/intersection already-accessed current-available)

        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))

        faceup-cards-buttons (map :title (faceup-accessible state already-accessed-fn))
        unrezzed-card "Unrezzed upgrade"
        root (root-content state :archives already-accessed-fn)
        unrezzed-cards-button (when (and (not no-root)
                                         (some (complement rezzed?) root))
                                [unrezzed-card])
        upgrade-buttons (when-not no-root
                          (->> root
                               (filter rezzed?)
                               (map :title)))
        facedown-card "Facedown card in Archives"
        facedown-cards-button (when (pos? (count (facedown-cards state already-accessed-fn)))
                                [facedown-card])
        everything-else "Everything else"
        everything-else-button (when (seq (clj-set/difference (set (map :cid (get-archives-inactive state)))
                                                              already-accessed))
                                 [everything-else])
        choices (concat faceup-cards-buttons
                        upgrade-buttons
                        facedown-cards-button
                        unrezzed-cards-button
                        everything-else-button)

        unrezzed-cards-fn
        (req (let [unrezzed-card (filter #(not (rezzed? %)) root)]
               (if (= 1 (count unrezzed-card))
                 ;; only one unrezzed upgrade; access it and continue
                 (let [already-accessed (conj already-accessed (:cid (first unrezzed-card)))
                       access-amount {:base base
                                      :total (dec total)}]
                   (wait-for (access-card state side (first unrezzed-card))
                             (continue-ability
                               state side
                               (access-helper-archives state access-amount already-accessed args)
                               nil nil)))
                 ;; more than one unrezzed upgrade. allow user to select with mouse.
                 (continue-ability
                   state side
                   {:async true
                    :prompt "Choose an upgrade in Archives to access."
                    :choices {:card #(and (= (second (:zone %)) :archives)
                                          (not (already-accessed %)))}
                    :effect (req (let [already-accessed (conj already-accessed (:cid target))
                                       access-amount {:base base
                                                      :total (dec total)}]
                                   (wait-for (access-card state side target)
                                             (continue-ability
                                               state side
                                               (access-helper-archives state access-amount already-accessed args)
                                               nil nil))))}
                   nil nil))))

        facedown-cards-fn
        (req (let [accessed (first (shuffle (facedown-cards state already-accessed)))
                   already-accessed (conj already-accessed (:cid accessed))
                   access-amount {:base (dec base)
                                  :total (dec total)}]
               (wait-for (access-card state side accessed)
                         (continue-ability
                           state side
                           (access-helper-archives state access-amount already-accessed args)
                           nil nil))))

        everything-else-fn
        (req (let [accessed (get-archives-inactive state)]
               (system-msg state side "accesses everything else in Archives")
               (wait-for (access-inactive-archives-cards state side accessed access-amount)
                         (let [already-accessed (apply conj already-accessed (map :cid async-result))
                               access-amount {:base (min 0 (- base (count async-result)))
                                              :total (min 0 (- total (count async-result)))}]
                           (continue-ability
                             state side
                             (access-helper-archives state access-amount already-accessed args)
                             nil nil)))))]
    (when (and (seq choices)
               (must-continue? state already-accessed-fn access-amount args))
      (cond
        (= choices unrezzed-cards-button)
        {:async true
         :effect unrezzed-cards-fn}

        (= choices facedown-cards-button)
        {:async true
         :effect facedown-cards-fn}

        (= choices everything-else-button)
        {:async true
         :effect everything-else-fn}

        ;; Present the normal options
        :else
        {:async true
         :prompt (str "Select a card to access. You must access all cards.")
         :choices choices
         :effect (req (cond

                        ;; accessing an unrezzed upgrade
                        (= target unrezzed-card)
                        (unrezzed-cards-fn state side eid nil nil)

                        ;; accessing a card that was added to archives because of the effect of another card
                        (= target facedown-card)
                        (facedown-cards-fn state side eid nil nil)

                        ;; accessing the "non-interactive" cards
                        (= target everything-else)
                        (everything-else-fn state side eid nil nil)

                        ;; accessing a rezzed upgrade, or a card in archives
                        :else
                        (let [accessed (some #(when (= (:title %) target) %)
                                             (concat (faceup-accessible state already-accessed-fn)
                                                     (root-content state :archives already-accessed-fn)))
                              already-accessed (conj already-accessed (:cid accessed))
                              ;; Base access count is only decremented when accessing a card in archives
                              access-amount {:base (if (in-discard? accessed) (dec base) base)
                                             :total (dec total)}]
                          (wait-for (access-card state side accessed)
                                    (continue-ability
                                      state side
                                      (access-helper-archives state access-amount already-accessed args)
                                      nil nil)))))}))))

(defmethod choose-access :archives
  [{:keys [base total] :as access-amount} server {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      total-cards (or (when only-card [only-card])
                                      (concat (get-in @state [:corp :discard])
                                              (when-not no-root
                                                (get-in @state [:corp :servers :archives :content]))))]
                  (cond
                    ;; Only 1 card to access
                    (and (= 1 (count total-cards))
                         (pos? total))
                    (access-card state side eid (first total-cards))

                    ;; At least 1 access
                    (pos? total)
                    (continue-ability
                      state side
                      (access-helper-archives state access-amount #{} args)
                      card nil)

                    ;; No accesses
                    :else
                    (effect-completed state side eid))))})

(defn max-access
  "Put an upper limit on the number of cards that can be accessed in this run. For Eater."
  [state side n]
  (swap! state assoc-in [:run :max-access] n))

(defn access-bonus
  "Increase the number of cards to be accessed in server during this run by n.
  For temporary/per-run effects like Legwork, Maker's Eye.
  Not for permanent increases like RDI."
  ([state side server bonus] (access-bonus state side server bonus (if (:run @state) :end-of-run :end-of-access)))
  ([state side server bonus duration]
   (let [floating-effect
         (register-floating-effect
           state side nil
           {:type :access-bonus
            :duration duration
            :req (req (= server (second targets)))
            :value bonus})])))

(defn access-bonus-count
  [state side s]
  (sum-effects state side nil :access-bonus [s]))

(defn access-count
  [state side kw]
  (let [run (:run @state)
        s (case kw
           :rd-access :rd
           :hq-access :hq
           kw)
        accesses (+ (get-in @state [:runner kw] 0)
                    (access-bonus-count state side s))]
    (if-let [max-access (:max-access run)]
      (min max-access accesses)
      accesses)))

(defmulti num-cards-to-access
  "Gets the list of cards to access for the server"
  (fn [state side server args]
    (if (get-only-card-to-access state)
      :only
      (get-server-type server))))

(defmethod num-cards-to-access :only
  [state side server {:keys [no-root]}]
  (let [card (get-only-card-to-access state)
        total-mod (access-count state side :total)
        sum (inc total-mod)
        total (min 1 (if-let [max-access (get-in @state [:run :max-access])]
                       (+ sum max-access)
                       sum))]
    {:base (if (installed? card) 0 1)
     :total total}))

(defmethod num-cards-to-access :remote
  [state side server args]
  (let [content (get-in @state [:corp :servers server :content])
        installed (->> (get-all-content content)
                       (filter #(can-access-loud state side %))
                       count)
        mod (access-count state side server)
        sum (+ installed mod)
        total-mod (access-count state side :total)
        total (if-let [max-access (get-in @state [:run :max-access])]
                (min (+ sum total-mod) (+ total-mod max-access))
                (+ sum total-mod))]
    {:base sum
     :total total}))

(let [location {:rd :deck
                :hq :hand
                :archives :discard}]
  (defn num-cards-central
    [state side base server access-key no-root]
    (let [mod (access-count state side access-key)
          sum (min (+ base mod) (count (get-in @state [:corp (get location server)])))
          root (get-in @state [:corp :servers server :content])
          installed (count (when-not no-root root))
          total-mod (access-count state side :total)
          total (if-let [max-access (get-in @state [:run :max-access])]
                  (min (+ sum installed total-mod) (+ total-mod max-access))
                  (+ sum installed total-mod))]
      {:base sum
       :total total})))

(defmethod num-cards-to-access :rd
  [state side server {:keys [no-root]}]
  (num-cards-central state side 1 :rd :rd-access no-root))

(defmethod num-cards-to-access :hq
  [state side server {:keys [no-root]}]
  (num-cards-central state side 1 :hq :hq-access no-root))

(defmethod num-cards-to-access :archives
  [state side server {:keys [no-root]}]
  (let [base (count (get-in @state [:corp :discard]))]
    (num-cards-central state side base :archives :archives no-root)))

(defn turn-archives-faceup
  [state side server]
  (when (= :archives (get-server-type (first server)))
    (doseq [card (get-in @state [:corp :discard])]
      (update! state side (assoc card :seen true)))))

(defn clean-access-args
  [{:keys [access-first] :as args}]
  (if access-first
    (assoc args :access-first
           (if (sequential? access-first)
             access-first
             [access-first]))
    args))

(defn do-access
  "Starts the access routines for the run's server."
  ([state side eid server] (do-access state side eid server nil))
  ([state side eid server {:keys [no-root access-first] :as args}]
   (wait-for (trigger-event-sync state side :pre-access (first server))
             (let [args (clean-access-args args)
                   access-amount (num-cards-to-access state side (first server) args)]
               (turn-archives-faceup state side server)
               (when (:run @state)
                 (swap! state assoc-in [:run :did-access] true))
               (when-not (zero? (:total access-amount))
                 (swap! state assoc-in [:runner :register :accessed-cards] true))
               (wait-for (resolve-ability state side (choose-access access-amount server (assoc args :server server)) nil nil)
                         (wait-for (trigger-event-sync state side :end-access-phase {:from-server (first server)})
                                   (unregister-floating-effects state side :end-of-access)
                                   (unregister-floating-events state side :end-of-access)
                                   (effect-completed state side eid)))))))
