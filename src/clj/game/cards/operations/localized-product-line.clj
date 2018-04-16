(in-ns 'game.core)

(def card-operations-localized-product-line
  {"Localized Product Line"
   {:prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :delayed-completion true
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