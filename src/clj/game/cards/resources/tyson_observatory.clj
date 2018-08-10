(in-ns 'game.cards.resources)

(def card-definition-tyson-observatory
  {"Tyson Observatory"
   {:abilities [{:cost [:click 2]
                 :label "Add hardware to grip from stack"
                 :prompt "Choose a piece of hardware"
                 :msg (msg "add " (:title target) " to their grip")
                 :choices (req (cancellable (filter #(is-type? % "Hardware") (:deck runner)) :sorted))
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (move target :hand))}]}})
