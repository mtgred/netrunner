(in-ns 'game.core)

(def card-operations-diversified-portfolio
  {"Diversified Portfolio"
   {:msg (msg "gain " (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))
              " [Credits]")
    :effect (effect (gain :credit (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))))}})
