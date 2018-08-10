(in-ns 'game.cards.identities)

(def card-definition-freedom-khumalo-crypto-anarchist
  {"Freedom Khumalo: Crypto-Anarchist"
   {:flags {:slow-trash (req true)}
    :interactions
    {:trash-ability
     {:interactive (req true)
      :async true
      :once :per-turn
      :label "[Freedom]: Trash card"
      :req (req (and (not (is-type? target "Agenda"))
                     (<= (:cost target)
                         (reduce + (map #(get-counters % :virus)
                                        (all-installed state :runner))))))
      :effect (req (let [accessed-card target
                         play-or-rez (:cost target)]
                     (show-wait-prompt state :corp "Runner to use Freedom Khumalo's ability")
                     (if (zero? play-or-rez)
                       (continue-ability state side
                                         {:async true
                                          :msg (msg "trash " (:title accessed-card) " at no cost")
                                          :effect (effect (clear-wait-prompt :corp)
                                                          (trash-no-cost eid accessed-card))}
                                         card nil)
                       (wait-for (resolve-ability state side (pick-virus-counters-to-spend play-or-rez) card nil)
                                 (do (clear-wait-prompt state :corp)
                                     (if-let [msg (:msg async-result)]
                                       (do (system-msg state :runner
                                                       (str "uses Freedom Khumalo: Crypto-Anarchist to"
                                                            " trash " (:title accessed-card)
                                                            " at no cost, spending " msg))
                                           (trash-no-cost state side eid accessed-card))
                                       ;; Player cancelled ability
                                       (do (swap! state dissoc-in [:per-turn (:cid card)])
                                           (access-non-agenda state side eid accessed-card :skip-trigger-event true))))))))}}}})
