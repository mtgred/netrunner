(in-ns 'game.cards.programs)

(def card-definition-rng-key
  {"RNG Key"
   {:events {:pre-access-card {:req (req (get-in card [:special :rng-guess]))
                               :async true
                               :msg (msg "to reveal " (:title target))
                               :effect (req (if-let [guess (get-in card [:special :rng-guess])]
                                              (if (installed? target)
                                                ;; Do not trigger on installed cards (can't "reveal" an installed card per UFAQ)
                                                (do (toast state :runner "Installed cards cannot be revealed, so RNG Key does not pay out." "info")
                                                    (effect-completed state side eid))
                                                (if (or (= guess (:cost target))
                                                        (= guess (:advancementcost target)))
                                                  (continue-ability state side
                                                                    {:prompt "Choose RNG Key award"
                                                                     :choices ["Gain 3 [Credits]" "Draw 2 cards"]
                                                                     :effect (req (if (= target "Draw 2 cards")
                                                                                    (do (draw state :runner 2)
                                                                                        (system-msg state :runner "uses RNG Key to draw 2 cards"))
                                                                                    (do (gain-credits state :runner 3)
                                                                                        (system-msg state :runner "uses RNG Key to gain 3 [Credits]"))))}
                                                                    card nil)
                                                  (effect-completed state side eid)))
                                              (effect-completed state side eid)))}
             :post-access-card {:effect (effect (update! (assoc-in card [:special :rng-guess] nil)))}
             :successful-run {:req (req (and (#{:hq :rd} target)
                                             (first-event? state :runner :successful-run #{[:hq] [:rd]})))
                              :optional {:prompt "Fire RNG Key?"
                                         :yes-ability {:prompt "Guess a number"
                                                       :choices {:number (req 20)}
                                                       :msg (msg "guess " target)
                                                       :effect (effect (update! (assoc-in card [:special :rng-guess] target)))}}}}}})
