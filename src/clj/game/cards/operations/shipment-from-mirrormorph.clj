(in-ns 'game.core)

(def card-operations-shipment-from-mirrormorph
  {"Shipment from MirrorMorph"
   (let [shelper (fn sh [n] {:prompt "Select a card to install with Shipment from MirrorMorph"
                             :priority -1
                             :delayed-completion true
                             :choices {:req #(and (= (:side %) "Corp")
                                                  (not (is-type? % "Operation"))
                                                  (in-hand? %))}
                             :effect (req (when-completed
                                            (corp-install state side target nil nil)
                                            (if (< n 3)
                                              (continue-ability state side (sh (inc n)) card nil)
                                              (effect-completed state side eid card))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (shelper 1) card nil))})})
