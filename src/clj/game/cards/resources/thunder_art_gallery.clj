(in-ns 'game.cards.resources)

(def card-definition-thunder-art-gallery
  {"Thunder Art Gallery"
   (let [first-event-check (fn [state fn1 fn2] (and (fn1 state :runner :runner-lose-tag #(= :runner (second %)))
                                            (fn2 state :runner :runner-prevent (fn [t] (seq (filter #(some #{:tag} %) t))))))
         ability {:choices {:req #(and (= "Runner" (:side %))
                                       (in-hand? %)
                                       (not (is-type? % "Event")))}
                  :async true
                  :prompt (msg "Select a card to install with Thunder Art Gallery")
                  :effect (req (if (and (runner-can-install? state side target)
                                        (can-pay? state side target
                                                  (install-cost state side target [:credit (dec (:cost target))])))
                                 (do (install-cost-bonus state side [:credit -1])
                                     (system-msg state side "uses Thunder Art Gallery to install a card.")
                                     (runner-install state side eid target nil))
                                 (effect-completed state side eid)))
                  :cancel-effect (effect (effect-completed eid))}]
     {:events {:runner-lose-tag (assoc ability :req (req (and (first-event-check state first-event? no-event?) (= side :runner))))
               :runner-prevent (assoc ability :req (req (and (first-event-check state no-event? first-event?) (seq (filter #(some #{:tag} %) targets)))))}})})
