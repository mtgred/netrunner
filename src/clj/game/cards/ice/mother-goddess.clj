(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-mother-goddess
  {"Mother Goddess"
   (let [ab (effect (update! (let [subtype (->> (mapcat :ices (flatten (seq (:servers corp))))
                                                (filter #(and (rezzed? %) (not= (:cid card) (:cid %))))
                                                (mapcat #(split (:subtype %) #" - "))
                                                (cons "Mythic")
                                                distinct
                                                (join " - "))]
                               (assoc card :subtype-target (remove-subtypes subtype "Mythic")
                                           :subtype subtype))))
         mg {:req (req (ice? target))
             :effect ab}]
     {:effect ab
      :subroutines [end-the-run]
      :events {:rez mg :card-moved mg :derez mg :ice-subtype-changed mg}})})
