(in-ns 'game.cards.operations)

(def card-definition-diversified-portfolio
  {"Diversified Portfolio"
   (letfn [(number-of-non-empty-remotes [state]
             (count (filter #(not (empty? %))
                            (map #(:content (second %))
                                 (get-remotes state)))))]
     {:msg (msg "gain " (number-of-non-empty-remotes state)
                " [Credits]")
      :effect (effect (gain-credits (number-of-non-empty-remotes state)))})})
