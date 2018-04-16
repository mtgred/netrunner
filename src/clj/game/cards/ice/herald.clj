(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-herald
  {"Herald"
   {:flags {:rd-reveal (req true)}
    :subroutines [(gain-credits 2)
                  {:label "Pay 1 [Credits] to place 1 advancement token on a card that can be advanced"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req can-be-advanced?}
                   :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    :access {:delayed-completion true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Herald subroutines")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Herald. Allow its subroutines to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (play-subroutine :corp eid {:card card :subroutine 0})
                                                                       (play-subroutine :corp eid {:card card :subroutine 1}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}})
