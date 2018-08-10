(in-ns 'game.cards.assets)

(def card-definition-contract-killer
  {"Contract Killer"
   {:advanceable :always
    :abilities [{:label "Trash a connection"
                 :async true
                 :cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 2))
                 :choices {:req #(has-subtype? % "Connection")}
                 :msg (msg "trash " (:title target))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (trash eid target nil))}
                {:label "Do 2 meat damage"
                 :async true
                 :cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 2))
                 :msg "do 2 meat damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage eid :meat 2 {:card card}))}]}})
