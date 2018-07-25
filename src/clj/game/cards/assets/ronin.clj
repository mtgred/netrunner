(in-ns 'game.cards.assets)

(def card-definition-ronin
  {"Ronin"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 4))
                 :msg "do 3 net damage"
                 :async true
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage eid :net 3 {:card card}))}]}})
