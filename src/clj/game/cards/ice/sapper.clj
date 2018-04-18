(in-ns 'game.core)

(def card-definitions-ice-sapper
  {"Sapper"
   {:flags {:rd-reveal (req true)}
    :subroutines [trash-program]
    :access {:delayed-completion true
             :req (req (and (not= (first (:zone card)) :discard)
                            (some #(is-type? % "Program") (all-active-installed state :runner))))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Sapper subroutine")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "Allow Sapper subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (req (clear-wait-prompt state :corp)
                                                                    (show-wait-prompt state :runner "Corp to trash a program with Sapper")
                                                                    (play-subroutine state :corp eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}})
