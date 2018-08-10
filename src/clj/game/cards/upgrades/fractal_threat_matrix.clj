(in-ns 'game.cards.upgrades)

(def card-definition-fractal-threat-matrix
  {"Fractal Threat Matrix"
   {:implementation "Manual trigger each time all subs are broken"
    :abilities [{:label "Trash the top 2 cards from the Stack"
                 :msg (msg (let [deck (:deck runner)]
                             (if (pos? (count deck))
                               (str "trash " (join ", " (map :title (take 2 deck))) " from the Stack")
                               "trash the top 2 cards from their Stack - but the Stack is empty")))
                 :effect (effect (mill :corp :runner 2))}]}})
