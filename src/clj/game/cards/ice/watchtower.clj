(in-ns 'game.cards.ice)

(def card-definition-watchtower
  {"Watchtower"
   {:subroutines [{:label "Search R&D and add 1 card to HQ"
                   :prompt "Choose a card to add to HQ"
                   :msg "add a card from R&D to HQ"
                   :choices (req (cancellable (:deck corp) :sorted))
                   :cancel-effect (effect (system-msg "cancels the effect of Watchtower"))
                   :effect (effect (shuffle! :deck)
                                   (move target :hand))}]}})
