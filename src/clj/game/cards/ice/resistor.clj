(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-resistor
  {"Resistor"
   {:effect (req (add-watch state (keyword (str "resistor" (:cid card)))
                            (fn [k ref old new]
                              (let [tags (get-in new [:runner :tag])]
                                (when (not= (get-in old [:runner :tag]) tags)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus tags))
                                  (update-ice-strength ref side (get-card ref card)))))))
    :strength-bonus (req (:tag runner))
    :leave-play (req (remove-watch state (keyword (str "resistor" (:cid card)))))
    :subroutines [(trace-ability 4 end-the-run)]}})
