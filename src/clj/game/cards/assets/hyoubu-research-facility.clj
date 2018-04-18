(in-ns 'game.core)

(def card-definitions-assets-hyoubu-research-facility
  {"Hyoubu Research Facility"
   {:events {:psi-bet-corp {:once :per-turn
                            :msg (msg "gain " target " [Credits]")
                            :effect (effect (gain :corp :credit target))}}}})
