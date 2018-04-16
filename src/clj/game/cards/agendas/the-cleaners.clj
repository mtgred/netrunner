(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-the-cleaners
  {"The Cleaners"
   {:events {:pre-damage {:req (req (and (= target :meat)
                                         (= side :corp)))
                          :msg "do 1 additional meat damage"
                          :effect (effect (damage-bonus :meat 1))}}}})