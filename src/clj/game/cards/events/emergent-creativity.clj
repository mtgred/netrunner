(in-ns 'game.core)

(def card-definitions-events-emergent-creativity
  {"Emergent Creativity"
   (letfn [(ec [trash-cost to-trash]
             {:delayed-completion true
             :prompt "Choose a hardware or program to install"
             :msg (msg "trash " (if (empty? to-trash) "no cards" (join ", " (map :title to-trash)))
                       " and install " (:title target) " lowering the cost by " trash-cost)
             :choices (req (cancellable (filter #(or (is-type? % "Program")
                                                     (is-type? % "Hardware"))
                                                (:deck runner)) :sorted))
             :effect (req (trigger-event state side :searched-stack nil)
                          (shuffle! state side :deck)
                          (doseq [c to-trash]
                            (trash state side c {:unpreventable true}))
                          (install-cost-bonus state side [:credit (- trash-cost)])
                          (runner-install state side target)
                          (effect-completed state side eid card))})]
   {:prompt "Choose Hardware and Programs to trash from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))
              :max (req (count (:hand runner)))}
    :cancel-effect (effect (resolve-ability (ec 0 []) card nil))
    :effect (req (let [trash-cost (apply + (map :cost targets))
                       to-trash targets]
                   (resolve-ability state side (ec trash-cost to-trash) card nil)))})})
