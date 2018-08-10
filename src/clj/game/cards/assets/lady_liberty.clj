(in-ns 'game.cards.assets)

(def card-definition-lady-liberty
  {"Lady Liberty"
   {:abilities [{:cost [:click 3]
                 :label "Add agenda from HQ to score area"
                 :req (req (let [counters (get-counters (get-card state card) :power)]
                             (some #(and (is-type? % "Agenda")
                                         (= counters (:agendapoints %)))
                                  (:hand corp))))
                 :async true
                 :effect (req (show-wait-prompt state :runner "Corp to select an agenda for Lady Liberty")
                              (continue-ability
                                state side
                                {:prompt "Select an Agenda in HQ to move to score area"
                                 :choices {:req #(and (is-type? % "Agenda")
                                                      (= (:agendapoints %) (get-counters (get-card state card) :power))
                                                      (in-hand? %))}
                                 :msg (msg "add " (:title target) " to score area")
                                 :async true
                                 :effect (req (wait-for (as-agenda state :corp target (:agendapoints target)
                                                                   {:register-events true})
                                                        (clear-wait-prompt state :runner)
                                                        (effect-completed state side eid)))}
                                card nil))}]
    :events {:corp-turn-begins {:effect (effect (add-counter card :power 1))}}}})
