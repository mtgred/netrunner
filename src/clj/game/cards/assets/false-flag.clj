(in-ns 'game.core)

(def card-definitions-assets-false-flag
  {"False Flag"
   (letfn [(tag-count [false-flag]
             (int (Math/floor (/ (:advance-counter false-flag 0)
                                 2))))]
     {:advanceable :always
      :access {:req (req (pos? (:advance-counter (get-card state card) 0)))
               :msg (msg "give the runner " (quantify (tag-count (get-card state card)) "tag"))
               :delayed-completion true
               :effect (effect (tag-runner :runner eid (tag-count (get-card state card))))}
      :abilities [{:cost [:click 1]
                   :advance-counter-cost 7
                   :label "Add False Flag to your score area as an agenda worth 3 agenda points"
                   :msg "add it to their score area as an agenda worth 3 agenda points"
                   :effect (effect (as-agenda :corp card 3))}]})})
