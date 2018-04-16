(in-ns 'game.core)

(declare run-event)

(def card-events-information-sifting
  {"Information Sifting"
   (letfn [(access-pile [cards pile pile-size]
             {:prompt "Choose a card to access. You must access all cards."
              :choices [(str "Card from pile " pile)]
              :delayed-completion true
              :effect (req (when-completed
                             (handle-access state side [(first cards)])
                             (if (< 1 (count cards))
                               (continue-ability state side (access-pile (next cards) pile pile-size) card nil)
                               (do (swap! state assoc-in [:run :cards-accessed] pile-size)
                                   (effect-completed state side eid card)))))})
           (which-pile [p1 p2]
             {:prompt "Choose a pile to access"
              :choices [(str "Pile 1 (" (count p1) " cards)") (str "Pile 2 (" (count p2) " cards)")]
              :delayed-completion true
              :effect (req (let [choice (if (.startsWith target "Pile 1") 1 2)]
                             (clear-wait-prompt state :corp)
                             (system-msg state side (str "chooses to access " target))
                             (continue-ability state side
                                (access-pile (if (= 1 choice) p1 p2) choice (count (if (= 1 choice) p1 p2)))
                                card nil)))})]
     (let [access-effect
           {:delayed-completion true
            :mandatory true
            :effect (req (if (< 1 (count (:hand corp)))
                           (do (show-wait-prompt state :runner "Corp to create two piles")
                               (continue-ability
                                 state :corp
                                 {:delayed-completion true
                                  :prompt (msg "Select up to " (dec (count (:hand corp))) " cards for the first pile")
                                  :choices {:req #(and (in-hand? %) (card-is? % :side :corp))
                                            :max (req (dec (count (:hand corp))))}
                                  :effect (effect (clear-wait-prompt :runner)
                                                  (show-wait-prompt :corp "Runner to select a pile")
                                                  (continue-ability
                                                    :runner
                                                    (which-pile (shuffle targets)
                                                                (shuffle (vec (clojure.set/difference
                                                                                (set (:hand corp)) (set targets)))))
                                                    card nil))
                                  } card nil))
                           (effect-completed state side eid card)))}]
       {:req (req hq-runnable)
        :effect (effect (run :hq {:req (req (= target :hq))
                                  :replace-access access-effect}
                             card))}))})
