(in-ns 'game.cards.hardware)

(def card-definition-obelus
  {"Obelus"
   {:in-play [:memory 1]
    :effect (req (gain state :runner :hand-size {:mod (:tag runner)})
                 (add-watch state :obelus
                   (fn [k ref old new]
                     (let [tagnew (get-in new [:runner :tag] 0)
                           tagold (get-in old [:runner :tag] 0)]
                       (when (> tagnew tagold)
                         (gain state :runner :hand-size {:mod (- tagnew tagold)}))
                       (when (< tagnew tagold)
                         (lose state :runner :hand-size {:mod (- tagold tagnew)}))))))
    :leave-play (req (remove-watch state :obelus)
                     (lose state :runner :hand-size {:mod (:tag runner)}))
    :events {:successful-run-ends {:once :per-turn
                                   :req (req (and (#{:rd :hq} (first (:server target)))
                                                  (first-event? state side :successful-run-ends
                                                                #(#{:rd :hq} (first (:server (first %)))))))
                                   :msg (msg "draw " (total-cards-accessed target) " cards")
                                   :effect (effect (draw (total-cards-accessed target)))}}}})
