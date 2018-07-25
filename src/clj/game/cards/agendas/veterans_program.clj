(in-ns 'game.cards.agendas)

(def card-definition-veterans-program
  {"Veterans Program"
   {:interactive (req true)
    :msg "lose 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
