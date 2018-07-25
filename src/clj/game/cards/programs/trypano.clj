(in-ns 'game.cards.programs)

(def card-definition-trypano
  {"Trypano"
   (let [trash-if-5 (req (when-let [h (get-card state (:host card))]
                           (if (and (>= (get-virus-counters state side card) 5)
                                      (not (and (card-flag? h :untrashable-while-rezzed true)
                                                (rezzed? h))))
                             (do (system-msg state :runner (str "uses Trypano to trash " (card-str state h)))
                                 (unregister-events state side card)
                                 (trash state :runner eid h nil))
                             (effect-completed state side eid))))]
       {:hosting {:req #(and (ice? %) (can-host? %))}
        :effect trash-if-5
        :events {:runner-turn-begins
                 {:optional {:prompt (msg "Place a virus counter on Trypano?")
                             :yes-ability {:effect (req (system-msg state :runner "places a virus counter on Trypano")
                                                        (add-counter state side card :virus 1))}}}
                 :counter-added {:async true
                                 :effect trash-if-5}
                 :card-moved {:effect trash-if-5
                              :async true}
                 :runner-install {:effect trash-if-5
                                  :async true}}})})
