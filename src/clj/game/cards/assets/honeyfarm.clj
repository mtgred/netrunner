(in-ns 'game.cards.assets)

(def card-definition-honeyfarm
  {"Honeyfarm"
   {:flags {:rd-reveal (req true)}
    :access {:msg "force the Runner to lose 1 [Credits]"
             :effect (effect (lose-credits :runner 1))}}})
