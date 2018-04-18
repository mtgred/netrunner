(in-ns 'game.core)

(def card-definitions-assets-sundew
  {"Sundew"
   {:events {:runner-spent-click {:once :per-turn
                                  :msg (req (when (not this-server) "gain 2 [Credits]"))
                                  :effect (req (when (not this-server)
                                                 (gain state :corp :credit 2)))}}}})
