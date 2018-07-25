(in-ns 'game.cards.upgrades)

(def card-definition-mason-bellamy
  {"Mason Bellamy"
   {:implementation "Manually triggered by Corp"
    :abilities [{:label "Force the Runner to lose [Click] after an encounter where they broke a subroutine"
                 :req (req this-server)
                 :msg "force the Runner to lose [Click]"
                 :effect (effect (lose :runner :click 1))}]}})
