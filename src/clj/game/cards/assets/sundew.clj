(in-ns 'game.cards.assets)

(def card-definition-sundew
  {"Sundew"
   {:implementation "Doesn't restrict credit gain"
    :events {:runner-spent-click {:once :per-turn
                                  :msg (req (when (not this-server) "gain 2 [Credits]"))
                                  :effect (req (when (not this-server)
                                                 (gain-credits state :corp 2)))}}}})
