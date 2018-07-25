(in-ns 'game.cards.agendas)

(def card-definition-project-beale
  {"Project Beale"
   {:interactive (req true)
    :agendapoints-runner (req 2)
    :effect (req (let [n (quot (- (get-counters card :advancement) 3) 2)]
                    (set-prop state side card
                              :counter {:agenda n}
                              :agendapoints (+ 2 n))))}})
