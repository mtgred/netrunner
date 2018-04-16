(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-hades-shard
  {"Hades Shard"
   (shard-constructor :archives "access all cards in Archives" {:delayed-completion true}
                      (req (trash state side card {:cause :ability-cost})
                           (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                           (when (:run @state)
                             (swap! state update-in [:run :cards-accessed] (fnil #(+ % (count (:discard corp))) 0)))
                           (when-completed (trigger-event-sync state side :pre-access :archives)
                                           (resolve-ability state :runner
                                                            (choose-access (get-in @state [:corp :discard])
                                                                           '(:archives)) card nil))))})