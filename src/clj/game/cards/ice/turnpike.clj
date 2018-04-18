(in-ns 'game.core)

(def card-definitions-ice-turnpike
  {"Turnpike"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "force the Runner to lose 1 [Credits]"
                 :effect (effect (lose :runner :credit 1))}]
    :subroutines [(tag-trace 5)]}})
