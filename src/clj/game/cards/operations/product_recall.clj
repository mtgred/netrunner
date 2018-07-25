(in-ns 'game.cards.operations)

(def card-definition-product-recall
  {"Product Recall"
   {:async true
    :prompt "Select a rezzed asset or upgrade to trash"
    :choices {:req #(and (rezzed? %)
                         (or (is-type? % "Asset")
                             (is-type? % "Upgrade")))}
    :effect (req (let [tcost (modified-trash-cost state side target)]
                   (wait-for (trash state side target {:unpreventable true})
                             (do (gain-credits state :corp tcost)
                                 (system-msg state side (str "uses Product Recall to trash " (card-str state target)
                                                             " and gain " tcost "[Credits]"))
                                 (effect-completed state side eid)))))}})
