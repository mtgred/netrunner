(in-ns 'game.core)

(def card-definitions-agendas-project-vitruvius
  {"Project Vitruvius"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (:advance-counter card) 3)))
    :abilities [{:counter-cost [:agenda 1]
                 :prompt "Choose a card in Archives to add to HQ"
                 :show-discard true
                 :choices {:req #(and (in-discard? %)
                                      (= (:side %) "Corp"))}
                 :req (req (< 0 (get-in card [:counter :agenda] 0)))
                 :msg (msg "add "
                           (if (:seen target)
                             (:title target) "an unseen card ")
                           " to HQ from Archives")
                 :effect (effect (move target :hand))}]}})
