(in-ns 'game.cards.agendas)

(def card-definition-astroscript-pilot-program
  {"AstroScript Pilot Program"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1] :msg (msg "place 1 advancement token on "
                                                      (card-str state target))
                 :choices {:req can-be-advanced?}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}})
