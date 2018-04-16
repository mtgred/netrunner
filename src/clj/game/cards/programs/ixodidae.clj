(in-ns 'game.core)

(declare can-host?)

(def card-programs-ixodidae
  {"Ixodidae"
   {:events {:corp-loss {:req (req (= (first target) :credit)) :msg "gain 1 [Credits]"
                         :effect (effect (gain :runner :credit 1))}
             :purge {:effect (effect (trash card))}}}})
