(in-ns 'game.core)

(def card-definitions-ice-chrysalis
  {"Chrysalis"
   {:flags {:rd-reveal (req true)}
    :subroutines [(do-net-damage 2)]
    :access {:delayed-completion true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Chrysalis subroutine")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Chrysalis. Allow its subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (play-subroutine eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}})
