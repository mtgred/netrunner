(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-panic-button
  {"Panic Button"
   {:init {:root "HQ"} :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                                    :req (req (and run (= (first (:server run)) :hq)))}]}})