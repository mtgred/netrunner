(in-ns 'game.cards.resources)

(def card-definition-chatterjee-university
  {"Chatterjee University"
   {:abilities [{:cost [:click 1]
                 :label "Place 1 power counter"
                 :msg "place 1 power counter on it"
                 :effect (effect (add-counter card :power 1))}
                {:cost [:click 1]
                 :label "Install a program from your Grip"
                 :prompt "Select a program to install from your Grip"
                 :choices {:req #(and (is-type? % "Program") (in-hand? %))}
                 :msg (msg "install " (:title target))
                 :effect (req (install-cost-bonus state side [:credit (- (get-counters card :power))])
                              (runner-install state side target)
                              (when (pos? (get-counters card :power))
                                (add-counter state side card :power -1)))}]}})
