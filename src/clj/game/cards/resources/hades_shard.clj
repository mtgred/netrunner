(in-ns 'game.cards.resources)

(def card-definition-hades-shard
  {"Hades Shard"
   (shard-constructor :archives "access all cards in Archives" {:async true}
                      (req (trash state side card {:cause :ability-cost})
                           (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                           (wait-for (trigger-event-sync state side :pre-access :archives)
                                     (resolve-ability state :runner
                                                      (choose-access (get-in @state [:corp :discard])
                                                                     '(:archives) {:no-root true}) card nil))))})
