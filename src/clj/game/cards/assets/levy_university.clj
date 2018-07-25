(in-ns 'game.cards.assets)

(def card-definition-levy-university
  {"Levy University"
   {:abilities [{:prompt "Choose an ICE"
                 :msg (msg "adds " (:title target) " to HQ")
                 :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                 :label "Search R&D for a piece of ICE"
                 :cost [:click 1 :credit 1]
                 :effect (effect (move target :hand)
                                 (shuffle! :deck))}]}})
