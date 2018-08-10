(in-ns 'game.cards.events)

(def card-definition-inject
  {"Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (is-type? c "Program")
                     (do (trash state side c {:unpreventable true})
                         (gain-credits state side 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}})
