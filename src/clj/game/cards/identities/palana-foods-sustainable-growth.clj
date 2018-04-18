(in-ns 'game.core)

(def card-definitions-identities-palana-foods-sustainable-growth
  {"Pālanā Foods: Sustainable Growth"
   {:events {:runner-draw {:req (req (and (first-event? state :corp :runner-draw)
                                          (pos? target)))
                           :msg "gain 1 [Credits]"
                           :effect (effect (gain :corp :credit 1))}}}})
