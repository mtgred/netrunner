(in-ns 'game.core)

(declare run-event)

(def card-events-inject
  {"Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (is-type? c "Program")
                     (do (trash state side c {:unpreventable true})
                         (gain state side :credit 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}})
