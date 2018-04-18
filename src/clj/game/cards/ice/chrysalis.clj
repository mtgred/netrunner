(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

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
