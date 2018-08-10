(in-ns 'game.cards.ice)

(def card-definition-turnpike
  {"Turnpike"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "force the Runner to lose 1 [Credits]"
                 :effect (effect (lose-credits :runner 1))}]
    :subroutines [(tag-trace 5)]}})
