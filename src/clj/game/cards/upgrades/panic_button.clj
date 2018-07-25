(in-ns 'game.cards.upgrades)

(def card-definition-panic-button
  {"Panic Button"
   {:init {:root "HQ"}
    :install-req (req (filter #{"HQ"} targets))
    :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                 :req (req (and run (= (first (:server run)) :hq)))}]}})
