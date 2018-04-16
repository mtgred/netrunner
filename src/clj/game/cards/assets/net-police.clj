(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-net-police
  {"Net Police"
   {:recurring (effect (set-prop card :rec-counter (:link runner)))
    :effect (effect (set-prop card :rec-counter (:link runner)))}})