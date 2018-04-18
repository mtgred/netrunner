(in-ns 'game.core)

(def card-definitions-ice-veritas
  {"Veritas"
   {:subroutines [{:label "Corp gains 2 [Credits]"
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain :corp :credit 2))}
                  {:label "Runner loses 2 [Credits]"
                   :msg "force the Runner to lose 2 [Credits]"
                   :effect (effect (lose :runner :credit 2))}
                  (trace-ability 2 give-tag)]}})
