(ns game.core.access
  (:require
    [game.core.agendas :refer [update-all-advancement-requirements update-all-agenda-points]]
    [game.core.board :refer [all-active]]
    [game.core.card :refer [agenda? condition-counter? corp? get-agenda-points get-card get-zone in-discard? in-hand? in-scored? operation? rezzed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [card-ability-cost trash-cost steal-cost]]
    [game.core.effects :refer [any-effects register-static-abilities register-lingering-effect sum-effects unregister-lingering-effects]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid]]
    [game.core.engine :refer [ability-as-handler can-trigger? checkpoint register-pending-event pay queue-event register-default-events resolve-ability should-trigger? trigger-event trigger-event-simult trigger-event-sync unregister-floating-events]]
    [game.core.finding :refer [find-cid]]
    [game.core.flags :refer [can-access? can-access-loud can-steal? can-trash? card-flag-fn? card-flag?]]
    [game.core.moving :refer [move trash]]
    [game.core.payment :refer [add-cost-label-to-ability build-cost-string can-pay? merge-costs ->c]]
    [game.core.revealing :refer [reveal]]
    [game.core.say :refer [play-sfx system-msg]]
    [game.core.servers :refer [get-server-type name-zone zone->name]]
    [game.core.update :refer [update!]]
    [game.utils :refer [quantify same-card?]]
    [game.macros :refer [continue-ability req wait-for]]
    [jinteki.utils :refer [add-cost-to-label]]
    [clojure.set :as clj-set]
    [medley.core :refer [find-first]]
    [clojure.string :as string]))

(defn no-trash-or-steal
  [state]
  (swap! state update-in [:runner :register :no-trash-or-steal] (fnil inc 0)))

(defn access-bonus-count
  [state side kw]
  (sum-effects state side :access-bonus kw))

(defn access-end
  "Trigger events involving the end of the access phase, including :no-trash and :post-access-card"
  ([state side eid c] (access-end state side eid c nil))
  ([state side eid c {:keys [trashed stolen]}]
   ;; Do not trigger :no-trash if card has already been trashed
   (wait-for (trigger-event-sync state side (when-not trashed :no-trash) c)
             (when (and (not trashed)
                        (not stolen)
                        ;; Don't increment :no-trash-or-steal if accessing a card in Archives
                        (not (in-discard? c)))
               (no-trash-or-steal state))
             (let [accessed-card (:access @state)]
               (swap! state dissoc :access)
               (trigger-event-sync state side eid :post-access-card c accessed-card)))))

;;; Accessing rules
(defn interactions
  [card ability-key]
  (get-in (card-def card) [:interactions ability-key]))

(defn- access-ab
  [card]
  (interactions card :access-ability))

(defn- access-ab-label
  [state card]
  (let [title (first (string/split (:title card) #":"))
        access-ability (access-ab card)
        ability (add-cost-label-to-ability access-ability (card-ability-cost state :runner access-ability card))
        label (add-cost-to-label ability)]
    (str "[" title "] " label)))

(defn access-non-agenda
  "Access a non-agenda. Show a prompt to trash for trashable cards."
  [state side eid c & {:keys [skip-trigger-event]}]
  (wait-for
    (trigger-event-sync state side (when-not skip-trigger-event :pre-trash) c)
    (swap! state update-in [:stats :runner :access :cards] (fnil inc 0))
    ;; Don't show the access prompt if:
    (if (or ;; 1) accessing seen cards in Archives
          (and (in-discard? c) (:seen c))
          ;; 2) Edward Kim's auto-trash flag is true
          (and (operation? c)
               (card-flag? c :can-trash-operation true))
          ;; 3) card has been moved to trash but hasn't been updated
          (and (not (in-discard? c))
               (find-cid (:cid c) (get-in @state [:corp :discard]))))
      (access-end state side eid c)
      ; Otherwise, show the access prompt
      (let [card (assoc c :seen true)
            ; Trash costs
            trash-cost (when-not (in-discard? c) (trash-cost state side card))
            trash-eid (assoc eid :source card :source-type :runner-trash-corp-cards)
            ; Runner cannot trash (eg Trebuchet)
            can-trash (can-trash? state side c)
            can-pay (when trash-cost
                      (can-pay? state :runner trash-eid card nil [(->c :credit trash-cost)]))
            trash-cost-str (when can-pay
                             [(str "Pay " trash-cost " [Credits] to trash")])
            ; Is the runner is forced to trash this card with only credits? (NAT)
            must-trash-with-credits? (and can-pay
                                          (get-in @state [:runner :register :must-trash-with-credits]))
            ; Access abilities
            access-ab-cards (when-not must-trash-with-credits?
                              (seq (filter #(and (can-trigger? state :runner eid (access-ab %) % [card])
                                                 (can-pay? state :runner eid % nil (card-ability-cost state side (access-ab %) % [card])))
                                           (all-active state :runner))))
            ; Remove any non-trash abilities, as they can't be used if we're forced to trash
            {trash-ab-cards true
             non-trash-ab-cards false} (group-by #(boolean (:trash? (access-ab %))) access-ab-cards)
            ; Is the runner is forced to trash this card by any means?
            ; Only relevant when not forced to trash with credits, as we want to include
            ; trash abilities here
            must-trash? (when-not must-trash-with-credits?
                          (and (or can-pay trash-ab-cards)
                               (card-flag-fn? state side card :must-trash true)))
            ; If we must trash, make the label only from the trash abilities
            ; If we're not allowed to trash, only use non-trash abilities
            ; Otherwise, make the label from all abilities
            ability-strs (mapv (fn [card]
                                 (let [label (access-ab-label state card)]
                                   {:cid (:cid card)
                                    :title label}))
                               (cond
                                 must-trash? trash-ab-cards
                                 (not can-trash) non-trash-ab-cards
                                 :else access-ab-cards))
            ; Only display "No action" when we're not forced to do anything, or
            ; if it is the only thing we can do
            forced-to-trash? (or must-trash? must-trash-with-credits?)
            no-action-str (when (or (not can-trash) (not forced-to-trash?))
                            ["No action"])
            choices (vec (concat ability-strs (when can-trash trash-cost-str) no-action-str))]
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
                          (let [card (update! state side (assoc c :seen true))]
                            (wait-for (pay state side (make-eid state trash-eid) card [(->c :credit trash-cost)])
                                      (when (:breach @state)
                                        (swap! state assoc-in [:breach :did-trash] true))
                                      (when (:run @state)
                                        (swap! state assoc-in [:run :did-trash] true)
                                        (when must-trash?
                                          (swap! state assoc-in [:run :did-access] true)))
                                      (swap! state assoc-in [:runner :register :trashed-card] true)
                                      (system-msg state side (str (:msg async-result) " to trash "
                                                                  (:title card) " from "
                                                                  (name-zone :corp (get-zone card))))
                                      (wait-for (trash state side card {:accessed true})
                                                (access-end state side eid (first async-result) {:trashed true}))))

                          ; Use access ability
                          (find-first #(same-card? % target) access-ab-cards)
                          (let [ability-card (find-first #(same-card? % target) access-ab-cards)
                                ability-eid (assoc eid :source ability-card :source-type :ability)
                                ability (access-ab ability-card)]
                            (when (and (:breach @state)
                                       (:trash? ability true))
                              (swap! state assoc-in [:breach :did-trash] true))
                            (when (and (:run @state)
                                       (:trash? ability true))
                              (swap! state assoc-in [:run :did-trash] true))
                            (wait-for (resolve-ability state side (make-eid state ability-eid) ability ability-card [card])
                                      (let [card (first async-result)]
                                        (access-end state side eid card {:trashed (in-discard? card)}))))))}
          card nil)))))

;;; Stealing agendas
(defn steal-cost-bonus
  "Applies a cost to the next steal attempt. costs can be a vector of [:key value] pairs,
  for example [(->c :credit 2) (->c :click 1)]."
  ([state _ costs] (steal-cost-bonus state nil costs nil))
  ([state _ costs source]
    (swap! state update-in [:bonus :steal-cost] #(conj % [costs source]))))

(defn steal
  "Moves a card to the runner's :scored area, triggering events from the completion of the steal."
  [state side eid card]
  (let [c (move state :runner (dissoc card :advance-counter :new) :scored {:force true})
        _ (when (card-flag? c :has-events-when-stolen true)
            (register-default-events state side c)
            (register-static-abilities state side c))
        _ (update-all-advancement-requirements state)
        _ (update-all-agenda-points state)
        c (get-card state c)
        points (get-agenda-points c)]
    (system-msg state :runner (str "steals " (:title c) " and gains " (quantify points "agenda point")))
    (swap! state update-in [:runner :register :stole-agenda] #(+ (or % 0) (:agendapoints c 0)))
    (play-sfx state side "agenda-steal")
    (when (:breach @state)
      (swap! state assoc-in [:breach :did-steal] true))
    (when (:run @state)
      (swap! state assoc-in [:run :did-steal] true))
    (when-let [on-stolen (:stolen (card-def c))]
      (register-pending-event state :agenda-stolen c on-stolen))
    (queue-event state :agenda-stolen {:card c
                                       :points points})
    (wait-for (checkpoint state nil (make-eid state eid) {:duration :agenda-stolen})
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
  (swap! state update-in [:stats :runner :access :cards] (fnil inc 0))
  (let [cost (merge-costs (steal-cost state side eid card))
        cost-strs (build-cost-string cost)
        can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs cost)) card (:title card) cost)
        can-steal (can-steal? state side card)
        ; Access abilities are useless in the discard
        access-ab-cards (when-not (in-discard? card)
                          (seq (filter #(and (can-trigger? state :runner eid (access-ab %) % [card])
                                             (can-pay? state :runner eid % nil (card-ability-cost state side (access-ab %) % [card])))
                                       (all-active state :runner))))
        ability-strs (mapv (fn [card]
                             (let [label (access-ab-label state card)]
                               {:cid (:cid card)
                                :title label}))
                           access-ab-cards)
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
                      (wait-for (pay state side
                                     (make-eid state
                                               (assoc eid :additional-costs cost
                                                      :source card
                                                      :source-type :runner-steal
                                                      :action :steal-cost))
                                     nil cost)
                                (system-msg state side (str (:msg async-result) " to steal "
                                                            (:title card) " from "
                                                            (name-zone :corp (get-zone card))))
                                (steal-agenda state side eid card))

                      ;; Use access ability
                      (find-first #(same-card? % target) access-ab-cards)
                      (let [ability-card (find-first #(same-card? % target) access-ab-cards)
                            ability-eid (assoc eid :source ability-card :source-type :ability)
                            ability (access-ab ability-card)]
                        (when (and (:breach @state)
                                   (:trash? ability true))
                          (swap! state assoc-in [:breach :did-trash] true))
                        (when (and (:run @state)
                                   (:trash? ability true))
                          (swap! state assoc-in [:run :did-trash] true))
                        (wait-for (resolve-ability state side (make-eid state ability-eid) ability ability-card [card])
                                  (let [card (first async-result)]
                                    (access-end state side eid card {:stolen (in-scored? card)}))))))}
      card nil)))

(defn- reveal-access?
  "Check if the card should be revealed on access"
  ;; If any special reveal message is wanted it can go in this function
  [state side {:keys [zone] :as card}]
  (let [cdef (card-def card)
        ;; Add more kw here as the maybe become relevant. Only think rd is relevant,
        ;; everything else should not be "unseen".
        reveal-kw (case (first zone)
                         :deck :rd-reveal
                         :hand :hq-reveal
                         :discard :archives-reveal
                         :reveal)]
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
  [state side eid {:keys [zone] :as card} title {:keys [cost-msg no-msg]}]
  (let [cost-str (join-cost-strs cost-msg)]
    (when-not no-msg
      (system-msg state side
                  (str (if (seq cost-msg)
                         (str cost-str " to access ")
                         "accesses ")
                       title
                       (when card
                         (str " from " (name-zone :corp zone)))))))
  (if (reveal-access? state side card)
    (do (system-msg state side (str "must reveal they accessed " (:title card)))
        (reveal state :runner eid card))
    (effect-completed state side eid)))

(defn access-ability
  [card cdef]
  (when-let [acc (:on-access cdef)]
    (assoc (ability-as-handler card acc)
           :condition :accessed)))

(defn installed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Corp needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (pos? cost) (assoc ability :cost [(->c :credit cost)]) ability)
         prompt (if (pos? cost)
                  (req (str "Pay " cost " [Credits] to use " (:title card) " ability?"))
                  (req (str "Use " (:title card) " ability?")))]
     (installed-access-trigger cost ab prompt)))
  ([cost ability prompt]
   (let [cost (if (number? cost) [(->c :credit cost)] cost)]
     {:on-access
      {:optional
       {:req (req (and installed (can-pay? state :corp eid card nil cost)))
        :waiting-prompt (:waiting-prompt ability)
        :prompt prompt
        :yes-ability (dissoc ability :waiting-prompt)}}})))

(defn- access-trigger-events
  "Trigger access effects, then move into trash/steal choice."
  [state side eid c title args]
  (let [cdef (card-def c)
        c (assoc c :seen (or (:seen c) (not (in-discard? c))))
        access-effect (access-ability c cdef)]
    (swap! state assoc-in [:runner :register :accessed-cards] true)
    (wait-for (msg-handle-access state side c title args)
              (wait-for (trigger-event-simult
                          state side :access
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
                          (access-end state side eid c
                                      {:trashed (find-cid (:cid c) (get-in @state [:corp :discard]))
                                       :stolen (and (agenda? c)
                                                    (find-cid (:cid c) (get-in @state [:runner :scored])))}))))))

(defn access-cost-bonus
  "Applies a cost to the next access. costs can be a vector of [:key value] pairs,
  for example [(->c :credit 2) (->c :click 1)]."
  [state _ costs]
  (swap! state update-in [:bonus :access-cost] #(merge-costs (concat % costs))))

(defn access-cost
  "Gets a vector of costs for accessing the given card."
  [state _]
  (merge-costs (get-in @state [:bonus :access-cost])))

(defn- refused-access-cost
  "The runner refused to pay (or could not pay) to access"
  [state side eid]
  (swap! state dissoc :access)
  (effect-completed state side eid))

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
           :effect (req (if (#{"OK" "No action"} target)
                          (refused-access-cost state side eid)
                          (wait-for (pay state side accessed-card cost)
                                    (if-let [payment-str (:msg async-result)]
                                      (access-trigger-events state side eid accessed-card title (assoc args :cost-msg payment-str))
                                      (refused-access-cost state side eid)))))})
        nil nil)
      ;; There are no access costs
      :else
      (access-trigger-events state side eid card title args))))

(defn access-card
  "Apply game rules for accessing the given card."
  ([state side eid card] (access-card state side eid card (:title card) nil))
  ([state side eid card title] (access-card state side eid card title nil))
  ([state side eid card title args]
   (when-not (in-discard? card)
     (swap! state update-in [:stats :runner :access :unique-cards] (fnil #(vec (distinct (conj % (:cid card)))) [])))
   ;; Indicate that we are in the access step.
   (swap! state assoc :access card)
   ;; Reset counters for increasing costs of trash, steal, and access.
   (swap! state update :bonus dissoc :trash :steal-cost :access-cost)
   (when (:breach @state)
     (let [zone (or (#{:discard :deck :hand} (first (get-zone card)))
                    (second (get-zone card)))]
       (swap! state update-in [:breach :cards-accessed zone] (fnil inc 0))))
   (when (:run @state)
     (let [zone (or (#{:discard :deck :hand} (first (get-zone card)))
                    (second (get-zone card)))]
       (swap! state update-in [:run :cards-accessed zone] (fnil inc 0))))
   ;; First trigger pre-access-card, then move to determining if we can trash or steal.
   (wait-for (trigger-event-sync state side :pre-access-card card)
             (access-pay state side eid card title args))))

(defn set-only-card-to-access
  [state _ card]
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
  (remove condition-counter? (concat content (get-all-hosted content))))

(defn- root-content
  ([state server]
   (->> (get-in @state [:corp :servers server :content])
        get-all-content
        (filter #(can-access? state :runner %))))
  ([state server already-accessed-fn]
   (remove already-accessed-fn (root-content state server))))

;;; Methods for allowing user-controlled multi-access in servers.
(defmulti must-continue?
  (fn [_state _already-accessed-fn _amount-access args]
    (get-server-type (first (:server args)))))

(defmethod must-continue? :remote
  [state already-accessed-fn access-amount args]
  (let [max-access (:max-access (:run @state))
        total-mod (:total-mod access-amount 0)
        limit-reached? (when max-access
                         (<= (+ max-access total-mod) (:chosen access-amount)))]
    (and (not (get-in @state [:run :prevent-access]))
         (not limit-reached?)
         (pos? (->> (get-in @state [:corp :servers (first (:server args)) :content])
                    get-all-content
                    (filter #(can-access? state :runner %))
                    (remove already-accessed-fn)
                    count
                    (+ total-mod))))))

(defn access-helper-remote
  [state {:keys [chosen] :as access-amount} already-accessed {:keys [server] :as args}]
  (let [current-available (set (map :cid (root-content state (first server))))
        already-accessed (clj-set/intersection already-accessed current-available)
        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))
        available (root-content state (first server) already-accessed-fn)]
    (when (and (seq available) (must-continue? state already-accessed-fn access-amount args))
      (if (= 1 (count available))
        {:async true
         :effect (req (wait-for (access-card state side (first available))
                                (continue-ability
                                 state side
                                 (access-helper-remote
                                  state {:total-mod (access-bonus-count state side :total)
                                         :chosen (inc chosen)}
                                  (conj already-accessed (:cid (first available)))
                                  args)
                                 nil nil)))}
        {:prompt "Click a card to access it. You must access all cards in this server."
         :choices {:card (fn [card] (some #(same-card? card %) available))}
         :async true
         :effect (req (wait-for (access-card state side target)
                                (continue-ability
                                 state side
                                 (access-helper-remote
                                   state {:total-mod (access-bonus-count state side :total)
                                          :chosen (inc chosen)}
                                   (conj already-accessed (:cid target)) args)
                                 card nil)))}))))

(defmulti choose-access
  ;; choose-access implements game prompts allowing the runner to choose the order of access
  (fn [_access-amount server _args]
    (get-server-type (first server))))

(defmethod choose-access :remote
  [{:keys [total-mod] :as access-amount} server args]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      max-access (:max-access (:run @state))
                      content (get-in @state [:corp :servers (first server) :content])
                      total-cards (or (when only-card [only-card])
                                      (->> (get-all-content content)
                                           (filter #(can-access-loud state side %))))
                      total-cards-count (count total-cards)
                      pos-max? (if max-access
                                 (pos? (+ max-access total-mod))
                                 true)
                      pos-total-cards? (pos? (+ total-cards-count total-mod))]

                  (cond
                    ;; Only 1 card to access
                    (and pos-max?
                         pos-total-cards?
                         only-card)
                    (access-card state side eid only-card)

                    ;; Normal access
                    (and pos-max?
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

(defmethod must-continue? :rd
  [state already-accessed-fn access-amount {:keys [no-root]}]
  (let [max-access (:max-access (:run @state))
        total-mod (:total-mod access-amount 0)
        limit-reached? (when max-access
                         (<= (+ max-access total-mod) (:chosen access-amount)))]
    (and (not (get-in @state [:run :prevent-access]))
         (not limit-reached?)
         (pos? (reduce + (concat (let [deck (access-cards-from-rd state)
                                       card-to-see (first (drop-while already-accessed-fn deck))]
                                   (when card-to-see
                                     [(:random-access-limit access-amount)]))
                                 (when-not no-root
                                   [(count (root-content state :rd already-accessed-fn))])
                                 [total-mod]))))))

(defn access-helper-rd
  [state {:keys [chosen random-access-limit] :as access-amount} already-accessed {:keys [no-root] :as args}]
  (let [current-available (set (concat (map :cid (get-in @state [:corp :deck]))
                                       (map :cid (root-content state :rd))))
        already-accessed (clj-set/intersection already-accessed current-available)
        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))

        deck (access-cards-from-rd state)
        card-to-access (first (drop-while already-accessed-fn deck))

        card-from "Card from deck"
        card-from-button (when (and (pos? random-access-limit)
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
                          ;; if R&D was shuffled because of the access,
                          ;; the runner "starts over" from the top
                          already-accessed (if shuffled-during-run
                                             (set (filter already-accessed-fn root))
                                             (conj already-accessed (:cid card-to-access)))]
                      (when shuffled-during-run
                        (swap! state update-in [:run :shuffled-during-access] dissoc :rd))
                      (continue-ability
                        state side
                        (access-helper-rd
                          state {:random-access-limit (dec random-access-limit)
                                 :total-mod (access-bonus-count state side :total)
                                 :chosen (inc chosen)}
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
                            state {:random-access-limit random-access-limit
                                   :total-mod (access-bonus-count state side :total)
                                   :chosen (inc chosen)}
                            (conj already-accessed (:cid (first unrezzed))) args)
                          nil nil))
              ;; more than one unrezzed upgrade. allow user to select with mouse.
              (continue-ability
                state side
                {:async true
                 :prompt "Choose an upgrade in root of R&D to access"
                 :choices {:card (fn [card] (some #(same-card? card %) unrezzed))}
                 :effect (req (wait-for (access-card state side target)
                                        (continue-ability
                                          state side
                                          (access-helper-rd
                                            state {:random-access-limit random-access-limit
                                                   :total-mod (access-bonus-count state side :total)
                                                   :chosen (inc chosen)}
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

        (and (= choices upgrade-buttons)
             (= 1 (count upgrade-buttons)))
        {:async true
         :effect (req (let [upgrade (first (filter rezzed? root))]
                        (wait-for (access-card state side upgrade)
                                  (continue-ability
                                   state side
                                   (access-helper-rd
                                    state {:random-access-limit random-access-limit
                                           :total-mod (access-bonus-count state side :total)
                                           :chosen (inc chosen)}
                                    (conj already-accessed (:cid upgrade)) args)
                                   nil nil))))}

        :else
        {:async true
         :prompt "Choose a card to access"
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
                                        state {:random-access-limit random-access-limit
                                               :total-mod (access-bonus-count state side :total)
                                               :chosen (inc chosen)}
                                        (conj already-accessed (:cid accessed)) args)
                                      card nil)))))}))))

(defmethod choose-access :rd
  [{:keys [random-access-limit total-mod] :as access-amount} _ {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      max-access (:max-access (:run @state))
                      total-cards (or (when only-card [only-card])
                                      (concat
                                        (take random-access-limit (access-cards-from-rd state))
                                        (when-not no-root
                                          (-> @state :corp :servers :rd :content))))
                      total-cards-count (count total-cards)
                      pos-max? (if max-access
                                 (pos? (+ max-access total-mod))
                                 true)
                      pos-total-cards? (pos? (+ total-cards-count total-mod))]

                  (cond
                    ;; Only 1 card to access
                    (and pos-max?
                         pos-total-cards?
                         only-card)
                    (access-card state side eid (first total-cards))

                    ;; Normal access
                    (and pos-max?
                         pos-total-cards?)
                    (continue-ability
                      state side
                      (access-helper-rd state access-amount #{} args)
                      card nil)

                    :else
                    (effect-completed state side eid))))})

(defmethod must-continue? :hq
  [state already-accessed-fn access-amount {:keys [no-root]}]
  (let [max-access (:max-access (:run @state))
        total-mod (:total-mod access-amount 0)
        limit-reached? (when max-access
                         (<= (+ max-access total-mod) (:chosen access-amount)))]
    (and (not (get-in @state [:run :prevent-access]))
         (not limit-reached?)
         (pos? (reduce + (concat (when-not (:prevent-hand-access (:run @state))
                                   (let [hand (get-in @state [:corp :hand])
                                         hand-candidates (remove already-accessed-fn hand)]
                                     [(min (:random-access-limit access-amount) (count hand-candidates))]))
                                 (when-not no-root
                                   [(count (root-content state :hq already-accessed-fn))])
                                 [total-mod]))))))

(defn- access-cards-from-hq
  [state]
  (let [f (get-in @state [:runner :hq-access-fn])]
    (f (get-in @state [:corp :hand]))))

(defn access-helper-hq
  [state {:keys [chosen random-access-limit] :as access-amount}
   already-accessed {:keys [no-root access-first] :as args}]
  (let [hand (when (not (:prevent-hand-access (:run @state)))
               (get-in @state [:corp :hand]))
        current-available (set (concat (map :cid hand)
                                       (map :cid (root-content state :hq))))
        already-accessed (clj-set/intersection already-accessed current-available)

        already-accessed-fn (fn [card] (contains? already-accessed (:cid card)))

        card-from "Card from hand"
        card-from-button (when (and (pos? random-access-limit)
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
        (req (if (any-effects state side :corp-choose-hq-access)
               ;; corp chooses access
               (continue-ability
                state :corp
                {:async true
                 :prompt (str "Choose a card in HQ for the Runner to access")
                 :waiting-prompt true
                 :choices {:all true
                           :card #(and (in-hand? %)
                                       (corp? %)
                                       (not (already-accessed-fn %)))}
                 :effect (req (wait-for (access-card state :runner target (:title target))
                                        (continue-ability
                                         state :runner
                                         (access-helper-hq
                                          state {:random-access-limit (dec random-access-limit)
                                                 :total-mod (access-bonus-count state side :total)
                                                 :chosen (inc chosen)}
                                          (conj already-accessed (:cid target)) args)
                                         card nil)))}
                card nil)
               ;; normal access
               (let [accessed (first (drop-while already-accessed-fn (access-cards-from-hq state)))]
                 (wait-for (access-card state side accessed (:title accessed))
                           (continue-ability
                            state side
                            (access-helper-hq
                             state {:random-access-limit (dec random-access-limit)
                                    :total-mod (access-bonus-count state side :total)
                                    :chosen (inc chosen)}
                             (conj already-accessed (:cid accessed)) args)
                            card nil)))))

        unrezzed-cards-fn
        (req
          (let [unrezzed (filter (complement rezzed?) root)]
            (if (= 1 (count unrezzed))
              ;; only one unrezzed upgrade; access it and continue
              (wait-for (access-card state side (first unrezzed))
                        (continue-ability
                          state side
                          (access-helper-hq
                            state {:random-access-limit random-access-limit
                                   :total-mod (access-bonus-count state side :total)
                                   :chosen (inc chosen)}
                            (conj already-accessed (:cid (first unrezzed))) args)
                          nil nil))
              ;; more than one unrezzed upgrade. allow user to select with mouse.
              (continue-ability
                state side
                {:async true
                 :prompt "Choose an upgrade in root of HQ to access"
                 :choices {:card (fn [card] (some #(same-card? card %) unrezzed))}
                 :effect (req (wait-for (access-card state side target)
                                        (continue-ability
                                          state side
                                          (access-helper-hq
                                            state {:random-access-limit random-access-limit
                                                   :total-mod (access-bonus-count state side :total)
                                                   :chosen (inc chosen)}
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
                                      state {:random-access-limit (dec random-access-limit)
                                             :total-mod (access-bonus-count state side :total)
                                             :chosen (inc chosen)}
                                      (conj already-accessed (:cid accessed))
                                      (assoc args :access-first (next access-first)))
                                    nil nil))))}

        (= choices card-from-button)
        {:async true
         :effect card-from-hand-fn}

        (= choices unrezzed-cards-button)
        {:async true
         :effect unrezzed-cards-fn}

        (and (= choices upgrade-buttons)
             (= 1 (count upgrade-buttons)))
        {:async true
         :effect (req (let [upgrade (first (filter rezzed? root))]
                        (wait-for (access-card state side upgrade)
                                  (continue-ability
                                    state side
                                    (access-helper-hq
                                      state {:random-access-limit random-access-limit
                                             :total-mod (access-bonus-count state side :total)
                                             :chosen (inc chosen)}
                                      (conj already-accessed (:cid upgrade)) args)
                                   nil nil))))}

        :else
        {:async true
         :prompt "Choose a card to access"
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
                                        state {:random-access-limit random-access-limit
                                               :total-mod (access-bonus-count state side :total)
                                               :chosen (inc chosen)}
                                        (conj already-accessed (:cid accessed)) args)
                                      card nil)))))}))))

(defmethod choose-access :hq
  [{:keys [total-mod] :as access-amount} _ {:keys [no-root] :as args}]
  {:async true
   :effect (req (wait-for (trigger-event-sync state side :candidates-determined :hq)
                          (let [only-card (get-only-card-to-access state)
                                max-access (:max-access (:run @state))
                                total-cards (or (when only-card [only-card])
                                                (concat
                                                 (when (not (:prevent-hand-access (:run @state)))
                                                   (get-in @state [:corp :hand]))
                                                 (when-not no-root
                                                   (root-content state :hq))))
                                total-cards-count (count total-cards)
                                pos-max? (if max-access
                                           (pos? (+ max-access total-mod))
                                           true)
                                pos-total-cards? (pos? (+ total-cards-count total-mod))]

                            (cond
                              ;; Only 1 card to access
                              (and pos-max?
                                   pos-total-cards?
                                   only-card)
                              (access-card state side eid (first total-cards))

                              ;; Normal access
                              (and pos-max?
                                   pos-total-cards?)
                              (continue-ability
                               state side
                               (access-helper-hq state access-amount #{} args)
                               card nil)

                              ;; No cards to access
                              :else
                              (effect-completed state side eid)))))})

(defn- accessible? [state card]
  (or (agenda? card)
      (should-trigger? state :corp (make-eid state) card nil (:on-access (card-def card)))))

(defn- get-archives-accessible [state]
  ;; only include agendas and cards with an :access ability that can trigger
  (filter #(and (:seen %) (accessible? state %)) (get-in @state [:corp :discard])))

(defn- get-archives-inactive [state]
  ;; get faceup cards with no access interaction
  (filter #(and (:seen %) (not (accessible? state %))) (get-in @state [:corp :discard])))

(defn- access-inactive-archives-cards
  ([state side eid cards access-amount] (access-inactive-archives-cards state side eid cards access-amount '()))
  ([state side eid cards {:keys [chosen] :as access-amount} accessed-cards]
   (let [max-access (:max-access (:run @state))
         total-mod (:total-mod access-amount 0)
         limit-reached? (when max-access
                          (<= (+ max-access total-mod) (:chosen access-amount)))]
     (if (and (seq cards) (not limit-reached?))
       (wait-for (access-card state side (first cards) nil {:no-msg true})
                 (let [access-amount {:total-mod (access-bonus-count state side :total)
                                      :chosen (inc chosen)}]
                   (access-inactive-archives-cards state side eid (next cards) access-amount (cons (first cards) accessed-cards))))
       (complete-with-result state side eid accessed-cards)))))

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
  [state already-accessed-fn access-amount {:keys [no-root]}]
  (let [max-access (:max-access (:run @state))
        total-mod (:total-mod access-amount 0)
        limit-reached? (when max-access
                         (<= (+ max-access total-mod) (:chosen access-amount)))]
    (and (not (get-in @state [:run :prevent-access]))
         (not limit-reached?)
         (pos? (->> (concat (get-in @state [:corp :discard])
                            (when-not no-root
                              (root-content state :archives)))
                    (remove already-accessed-fn)
                    count
                    (+ total-mod))))))

(defn access-helper-archives
  [state {:keys [chosen] :as access-amount} already-accessed {:keys [no-root] :as args}]
  (let [
        current-available (set (concat (map :cid (get-in @state [:corp :discard]))
                                       (map :cid (root-content state :archives))))
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
                       access-amount {:total-mod (access-bonus-count state side :total)
                                      :chosen (inc chosen)}]
                   (wait-for (access-card state side (first unrezzed-card))
                             (continue-ability
                               state side
                               (access-helper-archives state access-amount already-accessed args)
                               nil nil)))
                 ;; more than one unrezzed upgrade. allow user to select with mouse.
                 (continue-ability
                   state side
                   {:async true
                    :prompt "Choose an upgrade in Archives to access"
                    :choices {:card #(and (= :archives (second (get-zone %)))
                                          (not (already-accessed %)))}
                    :effect (req (let [already-accessed (conj already-accessed (:cid target))
                                       access-amount {:total-mod (access-bonus-count state side :total)
                                                      :chosen (inc chosen)}]
                                   (wait-for (access-card state side target)
                                             (continue-ability
                                               state side
                                               (access-helper-archives state access-amount already-accessed args)
                                               nil nil))))}
                   nil nil))))

        facedown-cards-fn
        (req (let [accessed (first (shuffle (facedown-cards state already-accessed-fn)))
                   already-accessed (conj already-accessed (:cid accessed))
                   access-amount {:total-mod (access-bonus-count state side :total)
                                  :chosen (inc chosen)}]
               (wait-for (access-card state side accessed)
                         (continue-ability
                           state side
                           (access-helper-archives state access-amount already-accessed args)
                           nil nil))))

        everything-else-fn
        (req (let [accessed (get-archives-inactive state)]
               (system-msg state side "accesses everything else in Archives")
               (wait-for (access-inactive-archives-cards state side accessed access-amount)
                         (let [already-accessed (apply conj already-accessed (keep :cid async-result))
                               access-amount {:total-mod (access-bonus-count state side :total)
                                              :chosen (+ chosen (count async-result))}]
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

        (and (= choices upgrade-buttons)
             (= 1 (count upgrade-buttons)))
        {:async true
         :effect (req (let [upgrade (first (filter rezzed? root))]
                        (wait-for (access-card state side upgrade)
                                  (continue-ability
                                   state side
                                   (access-helper-archives
                                    state {:total-mod (access-bonus-count state side :total)
                                           :chosen (inc chosen)}
                                    (conj already-accessed (:cid upgrade)) args)
                                   nil nil))))}

        (and (= choices faceup-cards-buttons)
             (= 1 (count faceup-cards-buttons)))
        {:async true
         :effect (req (let [card (first (faceup-accessible state already-accessed-fn))]
                        (wait-for (access-card state side card)
                                  (continue-ability
                                   state side
                                   (access-helper-archives
                                    state {:total-mod (access-bonus-count state side :total)
                                           :chosen (inc chosen)}
                                    (conj already-accessed (:cid card)) args)
                                   nil nil))))}

        (= choices facedown-cards-button)
        {:async true
         :effect facedown-cards-fn}

        (= choices everything-else-button)
        {:async true
         :effect everything-else-fn}

        ;; Present the normal options
        :else
        {:async true
         :prompt (str "Choose a card to access. You must access all cards")
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
                              access-amount {:total-mod (access-bonus-count state side :total)
                                             :chosen (inc chosen)}]
                          (wait-for (access-card state side accessed)
                                    (continue-ability
                                     state side
                                     (access-helper-archives state access-amount already-accessed args)
                                     nil nil)))))}))))

(defmethod choose-access :archives
  [{:keys [total-mod] :as access-amount} _ {:keys [no-root] :as args}]
  {:async true
   :effect (req (let [only-card (get-only-card-to-access state)
                      max-access (:max-access (:run @state))
                      total-cards (or (when only-card [only-card])
                                      (concat (get-in @state [:corp :discard])
                                              (when-not no-root
                                                (root-content state :archives))))
                      total-cards-count (count total-cards)
                      pos-max? (if max-access
                                 (pos? (+ max-access total-mod))
                                 true)
                      pos-total-cards? (pos? (+ total-cards-count total-mod))]
                  (cond
                    ;; Only 1 card to access
                    (and pos-max?
                         pos-total-cards?
                         only-card)
                    (access-card state side eid (first total-cards))

                    ;; At least 1 access
                    (and pos-max?
                         pos-total-cards?)
                    (continue-ability
                      state side
                      (access-helper-archives state access-amount #{} args)
                      card nil)

                    ;; No accesses
                    :else
                    (effect-completed state side eid))))})

(defn max-access
  "Put an upper limit on the number of cards that can be accessed in this run."
  [state n]
  (let [current-max (:max-access (:run @state))
        new-max (if current-max
                  (min current-max n)
                  n)]
    (swap! state assoc-in [:run :max-access] new-max)))

(defn access-bonus
  "Increase the number of cards to be accessed in server during this run by n.
  For temporary/per-run effects like Legwork, Maker's Eye."
  ([state side server bonus] (access-bonus state side server bonus (if (:run @state) :end-of-run :end-of-access)))
  ([state side server bonus duration]
   (let [floating-effect
         (register-lingering-effect
           state side nil
           {:type :access-bonus
            :duration duration
            :req (req (= server target))
            :value bonus})]
     floating-effect)))

(defmulti num-cards-to-access
  "Gets the list of cards to access for the server"
  (fn [state _ server _]
    (if (get-only-card-to-access state)
      :only
      (get-server-type server))))

(defmethod num-cards-to-access :only
  [state side _ _]
  (let [total-mod (access-bonus-count state side :total)]
    {:total-mod total-mod
     :chosen 0}))

(defmethod num-cards-to-access :remote
  [state side _ _]
  (let [total-mod (access-bonus-count state side :total)]
    {:total-mod total-mod
     :chosen 0}))

(defn num-cards-central
  [state side base access-key access-amount]
  (let [mod (access-bonus-count state side access-key)
        random-access-limit (+ base mod)
        total-mod (access-bonus-count state side :total)]
    {:random-access-limit (or access-amount
                              random-access-limit)
     :total-mod total-mod
     :chosen 0}))

(defmethod num-cards-to-access :rd
  [state side _ access-amount]
  (num-cards-central state side 1 :rd access-amount))

(defmethod num-cards-to-access :hq
  [state side _ access-amount]
  (num-cards-central state side 1 :hq access-amount))

(defmethod num-cards-to-access :archives
  [state side _ _]
  (let [total-mod (access-bonus-count state side :total)]
    {:total-mod total-mod
     :chosen 0}))

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

(defn access-n-cards
  "Access a specific number of cards from a server"
  [state side eid server n]
  (let [access-amount (num-cards-to-access state side (first server) n)]
    (when (:run @state)
      (swap! state assoc-in [:run :did-access] true)
      (max-access state n))
    (wait-for (resolve-ability state side (choose-access access-amount server {:server server}) nil nil)
              (unregister-lingering-effects state side :end-of-access)
              (unregister-floating-events state side :end-of-access)
              (effect-completed state side eid))))

(defn breach-server
  "Starts the breach routines for the run's server."
  ([state side eid server] (breach-server state side eid server nil))
  ([state side eid server args]
   (system-msg state side (str "breaches " (zone->name server)))
   (wait-for (trigger-event-simult state side :breach-server nil (first server))
             (swap! state assoc :breach {:breach-server (first server) :from-server (first server)})
             (let [args (clean-access-args args)
                   access-amount (num-cards-to-access state side (first server) nil)]
               (turn-archives-faceup state side server)
               (when (:run @state)
                 (swap! state assoc-in [:run :did-access] true))
               (wait-for (resolve-ability state side (choose-access access-amount server (assoc args :server server)) nil nil)
                         (wait-for (trigger-event-sync state side :end-breach-server (:breach @state))
                                   (swap! state assoc :breach nil)
                                   (unregister-lingering-effects state side :end-of-access)
                                   (unregister-floating-events state side :end-of-access)
                                   (effect-completed state side eid)))))))
