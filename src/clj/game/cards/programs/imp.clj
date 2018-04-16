(in-ns 'game.core)

(declare can-host?)

(def card-programs-imp
  {"Imp"
   {:flags {:slow-trash (req (pos? (get-in card [:counter :virus] 0)))}
    :data {:counter {:virus 2}}
    :abilities [{:counter-cost [:virus 1]
                 :msg "trash at no cost"
                 :once :per-turn
                 :effect (effect (trash-no-cost))}]}})