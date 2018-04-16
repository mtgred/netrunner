(in-ns 'game.core)

(def card-operations-product-recall
  {"Product Recall"
   {:prompt "Select a rezzed asset or upgrade to trash"
    :choices {:req #(and (rezzed? %)
                         (or (is-type? % "Asset") (is-type? % "Upgrade")))}
    :effect (req (let [c target]
                   (trigger-event state side :pre-trash c)
                   (let [tcost (trash-cost state side c)]
                     (trash state side c)
                     (gain state :corp :credit tcost)
                     (resolve-ability state side
                       {:msg (msg "trash " (card-str state c) " and gain " tcost " [Credits]")}
                      card nil)
                     (swap! state update-in [:bonus] dissoc :trash)
                     (effect-completed state side eid card))))}})
