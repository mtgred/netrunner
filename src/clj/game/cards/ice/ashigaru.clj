(in-ns 'game.core)

(def card-definitions-ice-ashigaru
  {"Ashigaru"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand corp)) " subroutines")}]
    :subroutines [end-the-run]}})
