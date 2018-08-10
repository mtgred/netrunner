(in-ns 'game.cards.hardware)

(def card-definition-mache
  {"Mâché"
   {:abilities [{:label "Draw 1 card"
                 :msg "draw 1 card"
                 :counter-cost [:power 3]
                 :effect (effect (draw :runner 1))}]
    :events {:runner-trash {:once :per-turn
                            :req (req (and (card-is? target :side :corp)
                                           (:access @state)
                                           (:trash target)))
                            :effect (effect (system-msg (str "places " (:trash target) " power counters on Mâché"))
                                            (add-counter card :power (:trash target)))}}}})
