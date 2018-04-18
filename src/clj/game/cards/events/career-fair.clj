(in-ns 'game.core)

(def card-definitions-events-career-fair
  {"Career Fair"
   {:prompt "Select a resource to install from your Grip"
    :choices {:req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}})
