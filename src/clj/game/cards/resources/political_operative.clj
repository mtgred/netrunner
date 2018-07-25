(in-ns 'game.cards.resources)

(def card-definition-political-operative
  {"Political Operative"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :abilities [{:prompt "Select a rezzed card with a trash cost"
                 :choices {:req #(and (:trash %)
                                      (rezzed? %))}
                 :effect (req (let [cost (modified-trash-cost state :runner target)]
                                (when (can-pay? state side nil [:credit cost])
                                  (resolve-ability
                                    state side
                                    {:msg (msg "pay " cost " [Credit] and trash " (:title target))
                                     :effect (effect (lose-credits cost)
                                                     (trash card {:cause :ability-cost})
                                                     (trash target))}
                                    card targets))))}]}})
