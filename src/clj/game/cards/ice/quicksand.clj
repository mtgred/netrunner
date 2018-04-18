(in-ns 'game.core)

(def card-definitions-ice-quicksand
  {"Quicksand"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (and this-server (= (dec (:position run)) (ice-index state card))))
                 :label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-all-ice))}]
    :subroutines [end-the-run]
    :strength-bonus (req (get-in card [:counter :power] 0))}})
