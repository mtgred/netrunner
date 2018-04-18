(in-ns 'game.core)

(def card-definitions-ice-nerine-20
  {"Nerine 2.0"
   {:subroutines [{:label "Do 1 brain damage and Corp may draw 1 card"
                   :delayed-completion true
                   :msg "do 1 brain damage"
                   :effect (req (when-completed (damage state :runner :brain 1 {:card card})
                                                (resolve-ability state side
                                                  {:optional
                                                   {:prompt "Draw 1 card?"
                                                    :yes-ability {:msg "draw 1 card"
                                                                  :effect (effect (draw))}
                                                    :no-ability {:effect (req (effect-completed state side eid))}}}
                                                 card nil)))}]
    :runner-abilities [(runner-break [:click 2] 2)]}})
