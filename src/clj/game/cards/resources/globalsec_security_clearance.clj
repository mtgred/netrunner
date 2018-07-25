(in-ns 'game.cards.resources)

(def card-definition-globalsec-security-clearance
  {"Globalsec Security Clearance"
   {:req (req (> (:link runner) 1))
    :flags {:runner-phase-12 (req true)}
    :abilities [{:msg "lose [Click] and look at the top card of R&D"
                 :once :per-turn
                 :effect (effect (prompt! card (str "The top card of R&D is "
                                                    (:title (first (:deck corp)))) ["OK"] {}))}]
    :events {:runner-turn-begins {:req (req (get-in @state [:per-turn (:cid card)]))
                                  :effect (effect (lose :click 1))}}}})
