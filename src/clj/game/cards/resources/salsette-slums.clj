(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-salsette-slums
  {"Salsette Slums"
   {:flags {:slow-trash (req true)}
    :implementation "Will not trigger Maw when used on card already trashed (2nd ability)"
    :events {:runner-install
             {:req (req (= card target))
              :silent (req true)
              :effect (effect (update! (assoc card :slums-active true)))}
             :runner-turn-begins
             {:effect (effect (update! (assoc card :slums-active true)))}
             :pre-trash
             {:req (req (and (:slums-active card)
                             (-> target card-def :flags :must-trash not)
                             (:trash target)
                             (= (:side target) "Corp")))
              :effect (req (toast state :runner (str "Click Salsette Slums to remove " (:title target)
                                                     " from the game") "info" {:prevent-duplicates true}))}}
    :abilities [{:label "Remove the currently accessed card from the game instead of trashing it"
                 :req (req (let [c (:card (first (get-in @state [:runner :prompt])))]
                             (if-let [trash-cost (trash-cost state side c)]
                               (if (can-pay? state :runner nil :credit trash-cost)
                                 (if (:slums-active card)
                                   true
                                   ((toast state :runner "Can only use a copy of Salsette Slums once per turn.") false))
                                 ((toast state :runner (str "Unable to pay for " (:title c) ".")) false))
                               ((toast state :runner "Not currently accessing a card with a trash cost.") false))))
                 :msg (msg (let [c (:card (first (get-in @state [:runner :prompt])))]
                             (str "pay " (trash-cost state side c) " [Credits] and remove " (:title c) " from the game")))
                 :effect (req (let [c (:card (first (get-in @state [:runner :prompt])))]
                                (deactivate state side c)
                                (move state :corp c :rfg)
                                (pay state :runner card :credit (trash-cost state side c))
                                (trigger-event state side :no-trash c)
                                (update! state side (dissoc card :slums-active))
                                (close-access-prompt state side)
                                (when-not (:run @state)
                                  (swap! state dissoc :access))))}
                {:label "Remove a card trashed this turn from the game"
                 :req (req (if (:slums-active card)
                             true
                             ((toast state :runner "Can only use a copy of Salsette Slums once per turn.") false)))
                 :effect (effect (resolve-ability
                                   {; only allow targeting cards that were trashed this turn -- not perfect, but good enough?
                                    :choices {:req #(some (fn [c] (= (:cid %) (:cid c)))
                                                          (map first (turn-events state side :runner-trash)))}
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (req (deactivate state side target)
                                                 (trigger-event state side :no-trash target)
                                                 (move state :corp target :rfg)
                                                 (update! state side (dissoc card :slums-active)))}
                                   card nil))}]}})