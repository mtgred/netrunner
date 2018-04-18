(in-ns 'game.core)

(def card-definitions-assets-honeyfarm
  {"Honeyfarm"
   {:flags {:rd-reveal (req true)}
    :access {:msg "force the Runner to lose 1 [Credits]"
             :effect (effect (lose :runner :credit 1))}}})
