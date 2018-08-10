(in-ns 'game.cards.ice)

(def card-definition-hive
  {"Hive"
   {:abilities [{:label "Gain subroutines"
                 :msg   (msg "gain " (min 5 (max 0 (- 5 (:agenda-point corp 0)))) " subroutines")}]
    :subroutines [end-the-run]}})
