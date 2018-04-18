(in-ns 'game.core)

(def card-definitions-programs-rng-key
  {"RNG Key"
   {:events {:pre-access-card {:req (req (get-in card [:special :rng-guess]))
                               :delayed-completion true
                               :msg (msg "to reveal " (:title target))
                               :effect (req (if-let [guess (get-in card [:special :rng-guess])]
                                              (if (or (= guess (:cost target))
                                                      (= guess (:advancementcost target)))
                                                (continue-ability state side
                                                                  {:prompt "Choose RNG Key award"
                                                                   :choices ["Gain 3 [Credits]" "Draw 2 cards"]
                                                                   :effect (req (if (= target "Draw 2 cards")
                                                                                  (do (draw state :runner 2)
                                                                                      (system-msg state :runner "uses RNG Key to draw 2 cards"))
                                                                                  (do (gain state :runner :credit 3)
                                                                                      (system-msg state :runner "uses RNG Key to gain 3 [Credits]"))))}
                                                                  card nil)
                                                (effect-completed state side eid))
                                              (effect-completed state side eid)))}
             :post-access-card {:effect (effect (update! (assoc-in card [:special :rng-guess] nil)))}
             :successful-run {:req (req (let [first-hq (first-successful-run-on-server? state :hq)
                                              first-rd (first-successful-run-on-server? state :rd)]
                                          (and first-hq first-rd (or (= target :hq) (= target :rd)))))
                              :optional {:prompt "Fire RNG Key?"
                                         :yes-ability {:prompt "Guess a number"
                                                       :choices {:number (req 20)}
                                                       :msg (msg "guess " target)
                                                       :effect (effect (update! (assoc-in card [:special :rng-guess] target)))}}}}}})
