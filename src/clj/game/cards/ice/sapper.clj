(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-sapper
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