(in-ns 'game.cards.assets)

(def card-definition-hyoubu-research-facility
  {"Hyoubu Research Facility"
   {:events {:psi-bet-corp {:once :per-turn
                            :msg (msg "gain " target " [Credits]")
                            :effect (effect (gain-credits :corp target))}}}})
