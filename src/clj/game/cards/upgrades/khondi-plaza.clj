(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-khondi-plaza
  {"Khondi Plaza"
   {:recurring (effect (set-prop card :rec-counter (count (get-remotes @state))))
    :effect (effect (set-prop card :rec-counter (count (get-remotes @state))))}})
