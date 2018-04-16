(in-ns 'game.core)

(def card-hardware-obelus
  {"Obelus"
   {:in-play [:memory 1]
    :effect (req (gain state :runner :hand-size-modification (:tag runner))
                 (add-watch state :obelus
                   (fn [k ref old new]
                     (let [tagnew (get-in new [:runner :tag])
                           tagold (get-in old [:runner :tag])]
                       (when (> tagnew tagold)
                         (gain state :runner :hand-size-modification (- tagnew tagold)))
                       (when (< tagnew tagold)
                         (lose state :runner :hand-size-modification (- tagold tagnew)))))))
    :leave-play (req (remove-watch state :obelus)
                     (lose state :runner :hand-size-modification (:tag runner)))
    :events {:successful-run-ends {:once :per-turn
                                   :req (req (let [successes (rest (turn-events state side :successful-run))]
                                               (and (#{[:rd] [:hq]} (:server target))
                                                    (empty? (filter #(#{'(:rd) '(:hq)} %) successes)))))
                                   :msg (msg "draw " (:cards-accessed target 0) " cards")
                                   :effect (effect (draw (:cards-accessed target 0)))}}}})
