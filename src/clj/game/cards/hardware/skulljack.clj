(in-ns 'game.core)

(def card-hardware-skulljack
  {"Skulljack"
   {:effect (effect (damage eid :brain 1 {:card card}))
    :events {:pre-trash {:effect (effect (trash-cost-bonus -1))}}}})