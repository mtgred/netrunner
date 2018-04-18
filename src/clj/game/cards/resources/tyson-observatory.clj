(in-ns 'game.core)

(def card-definitions-resources-tyson-observatory
  {"Tyson Observatory"
   {:abilities [{:prompt "Choose a piece of Hardware" :msg (msg "add " (:title target) " to their Grip")
                 :choices (req (cancellable (filter #(is-type? % "Hardware") (:deck runner)) :sorted))
                 :cost [:click 2]
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (move target :hand))}]}})
