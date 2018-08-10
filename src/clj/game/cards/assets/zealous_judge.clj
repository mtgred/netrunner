(in-ns 'game.cards.assets)

(def card-definition-zealous-judge
  {"Zealous Judge"
   {:rez-req (req tagged)
    :abilities [{:async true
                 :label "Give the Runner 1 tag"
                 :cost [:click 1 :credit 1]
                 :msg (msg "give the Runner 1 tag")
                 :effect (effect (gain-tags eid 1))}]}})
