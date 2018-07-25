(in-ns 'game.cards.assets)

(def card-definition-false-flag
  {"False Flag"
   (letfn [(tag-count [false-flag]
             (int (/ (get-counters false-flag :advancement) 2)))]
     {:advanceable :always
      :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
               :msg (msg "give the runner " (quantify (tag-count (get-card state card)) "tag"))
               :async true
               :effect (effect (gain-tags :corp eid (tag-count (get-card state card))))}
      :abilities [{:cost [:click 1]
                   :advance-counter-cost 7
                   :label "Add False Flag to your score area as an agenda worth 3 agenda points"
                   :msg "add it to their score area as an agenda worth 3 agenda points"
                   :async true
                   :effect (req (as-agenda state :corp eid card 3))}]})})
