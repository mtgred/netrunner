(in-ns 'game.core)

(declare run-event)

(def card-events-deja-vu
  {"Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :effect (req (move state side target :hand)
                 (when (has-subtype? target "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to their Grip")
                                     :choices (req (cancellable
                                                     (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                     :effect (effect (move target :hand))} card nil)))}})
