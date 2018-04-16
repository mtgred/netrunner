(in-ns 'game.core)

(def card-operations-freelancer
  {"Freelancer"
   {:req (req tagged)
    :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
    :choices {:max 2
              :req #(and (installed? %)
                         (is-type? % "Resource"))}
    :effect (final-effect (trash-cards :runner targets))}})