(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-chatterjee-university
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
                 :effect (req (install-cost-bonus state side [:credit (* -1 (get-in card [:counter :power] 0))])
                              (runner-install state side target)
                              (when (pos? (get-in card [:counter :power] 0))
                                (add-counter state side card :power -1)))}]}})