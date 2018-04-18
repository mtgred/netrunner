(in-ns 'game.core)

(def card-definitions-assets-net-police
  {"Net Police"
   {:recurring (effect (set-prop card :rec-counter (:link runner)))
    :effect (effect (set-prop card :rec-counter (:link runner)))}})
