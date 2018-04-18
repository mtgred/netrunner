(in-ns 'game.core)

(def card-definitions-assets-executive-search-firm
  {"Executive Search Firm"
   {:abilities [{:prompt "Choose an Executive, Sysop, or Character to add to HQ"
                 :msg (msg "add " (:title target) " to HQ and shuffle R&D")
                 :activatemsg "searches R&D for an Executive, Sysop, or Character"
                 :choices (req (cancellable (filter #(or (has-subtype? % "Executive")
                                                         (has-subtype? % "Sysop")
                                                         (has-subtype? % "Character"))
                                                    (:deck corp))
                                            :sorted))
                 :cost [:click 1]
                 :label "Search R&D for an Executive, Sysop, or Character"
                 :effect (effect (move target :hand) (shuffle! :deck))}]}})
