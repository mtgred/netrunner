(in-ns 'game.cards.ice)

(def card-definition-whirlpool
  {"Whirlpool"
   {:subroutines [{:msg "prevent the Runner from jacking out"
                   :effect (req (when (and (is-remote? (second (:zone card)))
                                           (> (count (concat (:ices (card->server state card))
                                                             (:content (card->server state card)))) 1))
                                  (prevent-jack-out state side))
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}})
