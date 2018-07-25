(in-ns 'game.cards.identities)

(def card-definition-jinteki-biotech-life-imagined
  {"Jinteki Biotech: Life Imagined"
   {:events {:pre-first-turn {:req (req (= side :corp))
                              :prompt "Choose a copy of Jinteki Biotech to use this game"
                              :choices ["The Brewery" "The Tank" "The Greenhouse"]
                              :effect (effect (update! (assoc card :biotech-target target))
                                              (system-msg (str "has chosen a copy of Jinteki Biotech for this game")))}}
    :abilities [{:label "Check chosen flip identity"
                 :req (req (:biotech-target card))
                 :effect (req (case (:biotech-target card)
                                "The Brewery"
                                (toast state :corp "Flip to: The Brewery (Do 2 net damage)" "info")
                                "The Tank"
                                (toast state :corp "Flip to: The Tank (Shuffle Archives into R&D)" "info")
                                "The Greenhouse"
                                (toast state :corp "Flip to: The Greenhouse (Place 4 advancement tokens on a card)" "info")))}
                {:cost [:click 3]
                 :req (req (not (:biotech-used card)))
                 :label "Flip this identity"
                 :effect (req (let [flip (:biotech-target card)]
                                (case flip
                                  "The Brewery"
                                  (do (system-msg state side "uses The Brewery to do 2 net damage")
                                      (damage state side eid :net 2 {:card card})
                                      (update! state side (assoc card :code "brewery")))
                                  "The Tank"
                                  (do (system-msg state side "uses The Tank to shuffle Archives into R&D")
                                      (shuffle-into-deck state side :discard)
                                      (update! state side (assoc card :code "tank")))
                                  "The Greenhouse"
                                  (do (system-msg state side (str "uses The Greenhouse to place 4 advancement tokens "
                                                                  "on a card that can be advanced"))
                                      (update! state side (assoc card :code "greenhouse"))
                                      (resolve-ability
                                        state side
                                        {:prompt "Select a card that can be advanced"
                                         :choices {:req can-be-advanced?}
                                         :effect (effect (add-prop target :advance-counter 4 {:placed true}))} card nil)))
                                (update! state side (assoc (get-card state card) :biotech-used true))))}]}})
