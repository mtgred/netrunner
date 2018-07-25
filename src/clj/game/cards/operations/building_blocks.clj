(in-ns 'game.cards.operations)

(def card-definition-building-blocks
  {"Building Blocks"
   {:choices {:req #(and (= (:side %) "Corp")
                         (has-subtype? % "Barrier")
                         (in-hand? %))}
    :async true
    :effect (req (corp-install state side eid target nil {:no-install-cost true :install-state :rezzed-no-cost}))}})
