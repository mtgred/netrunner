(in-ns 'game.core)

(def card-definitions-ice-hive
  {"Hive"
   {:abilities [{:label "Gain subroutines"
                 :msg   (msg "gain " (min 5 (max 0 (- 5 (:agenda-point corp 0)))) " subroutines")}]
    :subroutines [end-the-run]}})
