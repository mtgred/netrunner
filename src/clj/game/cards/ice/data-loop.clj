(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-data-loop
  {"Data Loop"
   {:implementation "Encounter effect is manual"
    :subroutines [end-the-run-if-tagged
                  end-the-run]
    :runner-abilities [{:label "Add 2 cards from your Grip to the top of the Stack"
                        :req (req (pos? (count (:hand runner))))
                        :effect (req (let [n (min 2 (count (:hand runner)))]
                                       (resolve-ability state side
                                         {:prompt (msg "Choose " n " cards in your Grip to add to the top of the Stack (first card targeted will be topmost)")
                                          :choices {:max n :all true
                                                    :req #(and (in-hand? %) (= (:side %) "Runner"))}
                                          :effect (req (doseq [c targets]
                                                         (move state :runner c :deck {:front true}))
                                                       (system-msg state :runner (str "adds " n " cards from their Grip to the top of the Stack")))}
                                        card nil)))}]}})
