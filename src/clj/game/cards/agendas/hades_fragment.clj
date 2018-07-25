(in-ns 'game.cards.agendas)

(def card-definition-hades-fragment
  {"Hades Fragment"
   {:flags {:corp-phase-12 (req (and (not-empty (get-in @state [:corp :discard]))
                                     (is-scored? state :corp card)))}
    :abilities [{:prompt "Select a card to add to the bottom of R&D"
                 :show-discard true
                 :choices {:req #(and (= (:side %) "Corp")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (move target :deck))
                 :msg (msg "add "
                           (if (:seen target)
                             (:title target)
                             "a card")
                           " to the bottom of R&D")}]}})
