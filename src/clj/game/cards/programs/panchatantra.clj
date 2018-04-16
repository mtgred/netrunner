(in-ns 'game.core)

(declare can-host?)

(def card-programs-panchatantra
  {"Panchatantra"
   {:abilities [{:msg "add a custom subtype to currently encountered ICE"
                 :once :per-turn}]}})
