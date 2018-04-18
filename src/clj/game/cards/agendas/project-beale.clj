(in-ns 'game.core)

(def card-definitions-agendas-project-beale
  {"Project Beale"
   {:interactive (req true)
    :agendapoints-runner (req 2)
    :effect (req (let [n (quot (- (:advance-counter card) 3) 2)]
                    (set-prop state side card
                              :counter {:agenda n}
                              :agendapoints (+ 2 n))))}})
