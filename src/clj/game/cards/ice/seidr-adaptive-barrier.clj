(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-seidr-adaptive-barrier
  {"Seidr Adaptive Barrier"
   {:effect (req (let [srv (second (:zone card))]
                   (add-watch state (keyword (str "sab" (:cid card)))
                              (fn [k ref old new]
                                (let [ices (count (get-in new [:corp :servers srv :ices]))]
                                  (when (not= (count (get-in old [:corp :servers srv :ices])) ices)
                                    (update! ref side (assoc (get-card ref card) :strength-bonus ices))
                                    (update-ice-strength ref side (get-card ref card))))))))
    :strength-bonus (req (count (:ices (card->server state card))))
    :leave-play (req (remove-watch state (keyword (str "sab" (:cid card)))))
    :subroutines [end-the-run]}})
