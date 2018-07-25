(in-ns 'game.cards.operations)

(def card-definition-localized-product-line
  {"Localized Product Line"
   {:prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :async true
    :effect (req (let [c (:title target)
                       cs (filter #(= (:title %) c) (:deck corp))]
                   (continue-ability
                    state side
                    {:prompt "How many copies?"
                     :choices {:number (req (count cs))}
                     :msg (msg "add " (quantify target "cop" "y" "ies") " of " c " to HQ")
                     :effect (req (shuffle! state :corp :deck)
                                  (doseq [c (take target cs)]
                                    (move state side c :hand)))}
                    card nil)))}})
