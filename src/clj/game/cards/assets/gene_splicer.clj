(in-ns 'game.cards.assets)

(def card-definition-gene-splicer
  {"Gene Splicer"
   {:advanceable :always
    :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
             :msg (msg "do " (get-counters (get-card state card) :advancement) " net damage")
             :async true
             :effect (effect (damage eid :net (get-counters (get-card state card) :advancement)
                                      {:card card}))}
    :abilities [{:cost [:click 1]
                 :advance-counter-cost 3
                 :label "Add Gene Splicing to your score area as an agenda worth 1 agenda point"
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :async true
                 :effect (req (as-agenda state :corp eid card 1))}]}})
