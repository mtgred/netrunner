(in-ns 'game.cards.upgrades)

(def card-definition-khondi-plaza
  {"Khondi Plaza"
   {:recurring (effect (set-prop card :rec-counter (count (get-remotes state))))
    :effect (effect (set-prop card :rec-counter (count (get-remotes state))))}})
