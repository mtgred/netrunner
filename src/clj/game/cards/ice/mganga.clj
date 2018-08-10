(in-ns 'game.cards.ice)

(def card-definition-mganga
  {"Mganga"
   {:subroutines [(do-psi {:label "do 2 net damage"
                           :async true
                           :player :corp
                           :effect (req (wait-for (damage state :corp :net 2 {:card card})
                                                  (trash state :corp eid card nil)))}
                          {:label "do 1 net damage"
                           :async true
                           :player :corp
                           :effect (req (wait-for (damage state :corp :net 1 {:card card})
                                                  (trash state :corp eid card nil)))})]}})
