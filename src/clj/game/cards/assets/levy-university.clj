(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-levy-university
  {"Levy University"
   {:abilities [{:prompt "Choose an ICE"
                 :msg (msg "adds " (:title target) " to HQ")
                 :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                 :label "Search R&D for a piece of ICE"
                 :cost [:click 1 :credit 1]
                 :effect (effect (shuffle! :deck)
                                 (move target :hand))}]}})