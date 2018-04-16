(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-chum
  {"Chum"
   {:subroutines [{:label "Give +2 strength to next ICE Runner encounters"
                   :req (req this-server)
                   :prompt "Select the ICE the Runner is encountering"
                   :choices {:req #(and (rezzed? %) (ice? %))}
                   :msg (msg "give " (:title target) " +2 strength")
                   :effect (req (let [ice (:cid target)]
                                  (register-events state side
                                    {:pre-ice-strength {:req (req (= (:cid target) ice))
                                                        :effect (effect (ice-strength-bonus 2 target))}
                                     :run-ends {:effect (effect (unregister-events card))}}
                                   card)
                                  (update-all-ice state side)))}
                  (do-net-damage 3)]
    :events {:pre-ice-strength nil :run-ends nil}}})
