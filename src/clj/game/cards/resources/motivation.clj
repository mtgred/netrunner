(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-motivation
  {"Motivation"
   (let [ability {:msg "look at the top card of their Stack"
                  :label "Look at the top card of Stack (start of turn)"
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (prompt! card (str "The top card of your Stack is "
                                                     (:title (first (:deck runner)))) ["OK"] {}))}]
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner)))}
    :events {:runner-turn-begins ability}
    :abilities [ability]})})
