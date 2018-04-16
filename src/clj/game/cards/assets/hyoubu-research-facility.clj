(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-hyoubu-research-facility
  {"Hyoubu Research Facility"
   {:events {:psi-bet-corp {:once :per-turn
                            :msg (msg "gain " target " [Credits]")
                            :effect (effect (gain :corp :credit target))}}}})
