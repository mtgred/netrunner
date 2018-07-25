(in-ns 'game.cards.assets)

(def card-definition-mark-yale
  {"Mark Yale"
   {:events {:agenda-counter-spent {:msg "gain 1 [Credits]"
                                    :effect (effect (gain-credits 1))}}
    :abilities [{:label "Trash to gain 2 [Credits]"
                 :msg "gain 2 [Credits]"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits 2))}
                {:label "Spend an agenda counter to gain 2 [Credits]"
                 :effect (effect (continue-ability
                                   {:prompt "Select an agenda with a counter"
                                    :choices {:req #(and (is-type? % "Agenda")
                                                         (pos? (get-counters % :agenda)))}
                                    :msg (msg "spend an agenda token on " (:title target) " and gain 2 [Credits]")
                                    :effect (effect (add-counter target :agenda -1)
                                                    (gain-credits 2)
                                                    (trigger-event :agenda-counter-spent card))}
                                   card nil))}]}})
