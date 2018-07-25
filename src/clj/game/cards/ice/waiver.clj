(in-ns 'game.cards.ice)

(def card-definition-waiver
  {"Waiver"
   {:subroutines [(trace-ability 5 {:label "Reveal the Runner's Grip and trash cards"
                                    :msg (msg "reveal all cards in the Runner's Grip: " (join ", " (map :title (:hand runner)))
                                              ". Cards with a play/install cost less than or equal to " (- target (second targets))
                                              " will be trashed")
                                    :effect (req (let [delta (- target (second targets))]
                                                   (doseq [c (:hand runner)]
                                                     (when (<= (:cost c) delta)
                                                       (resolve-ability
                                                         state side
                                                         {:msg (msg "trash " (:title c))
                                                          :effect (effect (trash c))}
                                                         card nil)))))})]}})
