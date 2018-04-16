(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-sundew
  {"Sundew"
   {:events {:runner-spent-click {:once :per-turn
                                  :msg (req (when (not this-server) "gain 2 [Credits]"))
                                  :effect (req (when (not this-server)
                                                 (gain state :corp :credit 2)))}}}})
