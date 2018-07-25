(in-ns 'game.cards.operations)

(def card-definition-audacity
  {"Audacity"
   (let [audacity (fn au [n] {:prompt "Choose a card on which to place an advancement"
                              :async true
                              :choices {:req can-be-advanced?}
                              :cancel-effect (req (effect-completed state side eid))
                              :msg (msg "place an advancement token on " (card-str state target))
                              :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                                           (if (< n 2)
                                             (continue-ability state side (au (inc n)) card nil)
                                             (effect-completed state side eid)))})]
   {:async true
    :req (req (let [h (:hand corp)
                    p (:play-area corp)]
                ;; this is needed to pass the req check for can-play? and again when card is actually played
                (if (some #(= (:cid %) (:cid card)) p)
                  (>= (count h) 2)
                  (>= (count h) 3))))
    :effect (req (system-msg state side "trashes all cards in HQ due to Audacity")
                 (doseq [c (:hand corp)]
                   (trash state side c {:unpreventable true}))
                 (continue-ability state side (audacity 1) card nil))})})
