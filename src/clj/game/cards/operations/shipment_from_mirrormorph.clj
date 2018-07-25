(in-ns 'game.cards.operations)

(def card-definition-shipment-from-mirrormorph
  {"Shipment from MirrorMorph"
   (let [shelper (fn sh [n] {:prompt "Select a card to install with Shipment from MirrorMorph"
                             :async true
                             :choices {:req #(and (= (:side %) "Corp")
                                                  (not (is-type? % "Operation"))
                                                  (in-hand? %))}
                             :effect (req (wait-for
                                            (corp-install state side target nil nil)
                                            (if (< n 3)
                                              (continue-ability state side (sh (inc n)) card nil)
                                              (effect-completed state side eid))))})]
     {:async true
      :effect (effect (continue-ability (shelper 1) card nil))})})
