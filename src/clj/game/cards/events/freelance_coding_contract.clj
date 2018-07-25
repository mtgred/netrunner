(in-ns 'game.cards.events)

(def card-definition-freelance-coding-contract
  {"Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Program")
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (trash state side c {:unpreventable true}))
                 (gain-credits state side (* 2 (count targets))))}})
