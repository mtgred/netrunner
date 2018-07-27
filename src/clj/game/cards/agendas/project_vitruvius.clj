(in-ns 'game.cards.agendas)

(def card-definition-project-vitruvius
  {"Project Vitruvius"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 3)))
    :abilities [{:counter-cost [:agenda 1]
                 :label "Add card in Archives to HQ"
                 :prompt "Choose a card in Archives to add to HQ"
                 :show-discard true
                 :choices {:req #(and (in-discard? %)
                                      (= (:side %) "Corp"))}
                 :req (req (pos? (get-counters card :agenda)))
                 :msg (msg "add "
                           (if (:seen target)
                             (:title target) "an unseen card ")
                           " to HQ from Archives")
                 :effect (effect (move target :hand))}]}})
