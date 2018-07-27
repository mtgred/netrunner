(in-ns 'game.cards.resources)

(def card-definition-artist-colony
  {"Artist Colony"
   {:abilities [{:label "Install card from stack"
                 :prompt "Choose a card to install"
                 :msg (msg "install " (:title target))
                 :req (req (not (install-locked? state side)))
                 :cost [:forfeit]
                 :choices (req (cancellable (filter #(not (is-type? % "Event")) (:deck runner)) :sorted))
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (runner-install target))}]}})
