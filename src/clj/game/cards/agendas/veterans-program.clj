(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-veterans-program
  {"Veterans Program"
   {:interactive (req true)
    :msg "lose 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})