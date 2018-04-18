(in-ns 'game.core)

(def card-definitions-upgrades-khondi-plaza
  {"Khondi Plaza"
   {:recurring (effect (set-prop card :rec-counter (count (get-remotes @state))))
    :effect (effect (set-prop card :rec-counter (count (get-remotes @state))))}})
