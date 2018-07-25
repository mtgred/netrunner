(in-ns 'game.cards.events)

(def card-definition-modded
  {"Modded"
   {:prompt "Select a program or piece of hardware to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}})
