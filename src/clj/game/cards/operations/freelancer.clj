(in-ns 'game.cards.operations)

(def card-definition-freelancer
  {"Freelancer"
   {:req (req tagged)
    :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
    :choices {:max 2
              :req #(and (installed? %)
                         (is-type? % "Resource"))}
    :effect (effect (trash-cards :runner targets))}})
