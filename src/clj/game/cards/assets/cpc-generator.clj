(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-cpc-generator
  {"CPC Generator"
   {:events {:runner-click-credit {:req (req (first-event? state side :runner-click-credit))
                                   :msg "gain 1 [Credits]"
                                   :effect (effect (gain :corp :credit 1))}}}})