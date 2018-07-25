(in-ns 'game.cards.agendas)

(def card-definition-the-cleaners
  {"The Cleaners"
   {:events {:pre-damage {:req (req (and (= target :meat)
                                         (= side :corp)))
                          :msg "do 1 additional meat damage"
                          :effect (effect (damage-bonus :meat 1))}}}})
