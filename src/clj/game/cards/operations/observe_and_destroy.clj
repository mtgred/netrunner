(in-ns 'game.cards.operations)

(def card-definition-observe-and-destroy
  {"Observe and Destroy"
   {:additional-cost [:tag 1]
    :req (req (and (pos? (:tag runner))
                   (< (:credit runner) 6)))
    :async true
    :effect (effect (continue-ability
                      {:prompt "Select an installed card to trash"
                       :choices {:req installed?}
                       :msg (msg "remove 1 Runner tag and trash " (:title target))
                       :effect (effect (trash target))}
                     card nil))}})
