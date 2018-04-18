(in-ns 'game.core)

(def card-definitions-ice-chiyashi
  {"Chiyashi"
   {:implementation "Trash effect when using an AI to break is activated manually"
    :abilities [{:label "Trash the top 2 cards of the Runner's Stack"
                 :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                 :msg (msg (str "trash " (join ", " (map :title (take 2 (:deck runner)))) " from the Runner's Stack"))
                 :effect (effect (mill :corp :runner 2))}]
    :subroutines [(do-net-damage 2)
                  end-the-run]}})
