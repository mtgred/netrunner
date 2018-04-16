(in-ns 'game.core)

(declare run-event)

(def card-events-out-of-the-ashes
  {"Out of the Ashes"
   (let [ashes-run {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :delayed-completion true
                    :effect (effect (run eid target nil card))}
         ashes-recur (fn ashes-recur [n]
                       {:prompt "Remove Out of the Ashes from the game to make a run?"
                        :choices ["Yes" "No"]
                        :effect (req (if (= target "Yes")
                                       (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard runner))]
                                         (system-msg state side "removes Out of the Ashes from the game to make a run")
                                         (move state side card :rfg)
                                         (unregister-events state side card)
                                         (when-completed (resolve-ability state side ashes-run card nil)
                                                         (if (< 1 n)
                                                           (continue-ability state side (ashes-recur (dec n)) card nil)
                                                           (effect-completed state side eid card))))))})
         ashes-flag {:runner-phase-12 {:priority -1
                                       :once :per-turn
                                       :once-key :out-of-ashes
                                       :effect (effect (continue-ability
                                                         (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                                     (:discard runner))))
                                                         card nil))}}]
   (run-event
    {:move-zone (req (if (= [:discard] (:zone card))
                       (register-events state side ashes-flag (assoc card :zone [:discard]))
                       (unregister-events state side card)))
     :events {:runner-phase-12 nil}}
    nil))})