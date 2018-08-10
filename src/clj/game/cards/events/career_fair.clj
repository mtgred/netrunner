(in-ns 'game.cards.events)

(def card-definition-career-fair
  {"Career Fair"
   {:prompt "Select a resource to install from your Grip"
    :choices {:req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}})
