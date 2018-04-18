(in-ns 'game.core)

(def card-definitions-assets-gene-splicer
  {"Gene Splicer"
   {:advanceable :always
    :access {:req (req (< 0 (:advance-counter (get-card state card) 0)))
             :msg (msg "do " (:advance-counter (get-card state card) 0) " net damage")
             :delayed-completion true
             :effect (effect (damage eid :net (:advance-counter (get-card state card) 0)
                                      {:card card}))}
    :abilities [{:cost [:click 1]
                 :advance-counter-cost 3
                 :label "Add Gene Splicing to your score area as an agenda worth 1 agenda point"
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :effect (effect (as-agenda :corp card 1))}]}})
