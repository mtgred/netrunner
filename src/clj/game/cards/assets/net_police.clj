(in-ns 'game.cards.assets)

(def card-definition-net-police
  {"Net Police"
   {:recurring (effect (set-prop card :rec-counter (:link runner)))
    :effect (effect (set-prop card :rec-counter (:link runner)))}})
