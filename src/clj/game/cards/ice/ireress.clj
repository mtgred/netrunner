(in-ns 'game.core)

(def card-definitions-ice-ireress
  {"Ireress"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:bad-publicity corp 0) " subroutines")}]
    :subroutines [{:msg "make the Runner lose 1 [Credits]"
                   :effect (effect (lose :runner :credit 1))}]}})
