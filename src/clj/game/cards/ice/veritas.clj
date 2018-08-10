(in-ns 'game.cards.ice)

(def card-definition-veritas
  {"Veritas"
   {:subroutines [{:label "Corp gains 2 [Credits]"
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain-credits :corp 2))}
                  {:label "Runner loses 2 [Credits]"
                   :msg "force the Runner to lose 2 [Credits]"
                   :effect (effect (lose-credits :runner 2))}
                  (trace-ability 2 (give-tags 1))]}})
