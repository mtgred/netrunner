(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-political-operative
  {"Political Operative"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :abilities [{:prompt "Select a rezzed card with a trash cost"
                 :choices {:req #(and (:trash %) (rezzed? %))}
                 :effect (req (let [c target]
                                (trigger-event state side :pre-trash c)
                                (let [cost (trash-cost state :runner c)]
                                  (when (can-pay? state side nil [:credit cost])
                                    (resolve-ability
                                      state side
                                      {:msg (msg "pay " cost " [Credit] and trash " (:title c))
                                       :effect (effect (lose :credit cost)
                                                       (trash card {:cause :ability-cost})
                                                       (trash c))}
                                     card nil)))
                                (swap! state update-in [:bonus] dissoc :trash)))}]}})
