(in-ns 'game.core)

(def card-definitions-ice-mganga
  {"Mganga"
   {:subroutines [(do-psi {:label "do 2 net damage"
                           :delayed-completion true
                           :player :corp
                           :effect (req (when-completed (damage state :corp :net 2 {:card card})
                                                        (trash state :corp eid card nil)))}
                          {:label "do 1 net damage"
                           :delayed-completion true
                           :player :corp
                           :effect (req (when-completed (damage state :corp :net 1 {:card card})
                                                        (trash state :corp eid card nil)))})]}})
