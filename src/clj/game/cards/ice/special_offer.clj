(in-ns 'game.cards.ice)

(def card-definition-special-offer
  {"Special Offer"
   {:subroutines [{:label "Gain 5 [Credits] and trash Special Offer"
                   :effect (req (gain-credits state :corp 5)
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card)
                                (system-msg state side (str "gains 5 [Credits] and trashes Special Offer")))}]}})
