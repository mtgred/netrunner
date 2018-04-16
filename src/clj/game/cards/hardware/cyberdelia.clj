(in-ns 'game.core)

(def card-hardware-cyberdelia
  {"Cyberdelia"
   {:implementation "Credit gain is manually triggered."
    :in-play [:memory 1]
    :abilities [{:msg "gain 1 [Credits] for breaking all subroutines on a piece of ice"
                 :once :per-turn
                 :effect (effect (gain :credit 1))}]}})
