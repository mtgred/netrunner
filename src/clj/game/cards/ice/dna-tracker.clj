(in-ns 'game.core)

(def card-definitions-ice-dna-tracker
  {"DNA Tracker"
   {:subroutines [{:msg "do 1 net damage and make the Runner lose 2 [Credits]"
                   :effect (req (when-completed (damage state side :net 1 {:card card})
                                                (lose state :runner :credit 2)))}]}})
