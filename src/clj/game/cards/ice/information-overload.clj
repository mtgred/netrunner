(in-ns 'game.core)

(def card-definitions-ice-information-overload
  {"Information Overload"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:tag runner 0) " subroutines")}
                (tag-trace 1)]
    :subroutines [trash-installed]}})
