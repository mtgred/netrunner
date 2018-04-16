(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-accelerated-beta-test
  {"Accelerated Beta Test"
   (letfn [(abt [n i]
             (if (pos? i)
               {:delayed-completion true
                :prompt "Select a piece of ICE from the Temporary Zone to install"
                :choices {:req #(and (= (:side %) "Corp")
                                     (ice? %)
                                     (= (:zone %) [:play-area]))}
                :effect (req (when-completed (corp-install state side target nil
                                                           {:no-install-cost true :install-state :rezzed-no-cost})
                                             (let [card (get-card state card)]
                                               (unregister-events state side card)
                                               (if (not (:shuffle-occurred card))
                                                 (if (< n i)
                                                   (continue-ability state side (abt (inc n) i) card nil)
                                                   (do (doseq [c (get-in @state [:corp :play-area])]
                                                         (system-msg state side "trashes a card")
                                                         (trash state side c {:unpreventable true}))
                                                       (effect-completed state side eid)))
                                                 (do (doseq [c (get-in @state [:corp :play-area])]
                                                       (move state side c :deck))
                                                     (shuffle! state side :deck)
                                                     (effect-completed state side eid))))))
                :cancel-effect (req (doseq [c (get-in @state [:corp :play-area])]
                                      (system-msg state side "trashes a card")
                                      (trash state side c {:unpreventable true})))}
               {:prompt "None of the cards are ice. Say goodbye!"
                :choices ["I have no regrets"]
                :effect (req (doseq [c (get-in @state [:corp :play-area])]
                               (system-msg state side "trashes a card")
                               (trash state side c {:unpreventable true})))}))]
     {:interactive (req true)
      :optional {:prompt "Look at the top 3 cards of R&D?"
                 :yes-ability {:delayed-completion true
                               :msg "look at the top 3 cards of R&D"
                               :effect (req (register-events state side
                                                             {:corp-shuffle-deck
                                                              {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                             card)
                                            (let [n (count (filter ice? (take 3 (:deck corp))))]
                                              (doseq [c (take (min (count (:deck corp)) 3) (:deck corp))]
                                                (move state side c :play-area))
                                              (continue-ability state side (abt 1 n) card nil)))}}})})
