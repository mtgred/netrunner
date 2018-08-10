(in-ns 'game.cards.events)

(def card-definition-high-stakes-job
  {"High-Stakes Job"
   (run-event
    {:choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                         bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:corp :servers])))]
                     (zones->sorted-names (remove (set bad-zones) (get-runnable-zones state)))))}
    {:end-run {:req (req (:successful run))
               :msg "gain 12 [Credits]"
               :effect (effect (gain-credits :runner 12))}})})
