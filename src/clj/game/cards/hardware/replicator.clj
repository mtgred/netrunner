(in-ns 'game.core)

(def card-hardware-replicator
  {"Replicator"
   (letfn [(hardware-and-in-deck? [target runner]
             (and (is-type? target "Hardware")
                  (some #(= (:title %) (:title target)) (:deck runner))))]
     {:events {:runner-install
               {:interactive (req (hardware-and-in-deck? target runner))
                :silent (req (not (hardware-and-in-deck? target runner)))
                :optional {:prompt "Use Replicator to add a copy?"
                           :req (req (hardware-and-in-deck? target runner))
                           :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                         :effect (effect (trigger-event :searched-stack nil)
                                                   (shuffle! :deck)
                                                   (move (some #(when (= (:title %) (:title target)) %)
                                                               (:deck runner)) :hand))}}}}})})
