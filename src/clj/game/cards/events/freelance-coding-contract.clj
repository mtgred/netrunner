(in-ns 'game.core)

(declare run-event)

(def card-events-freelance-coding-contract
  {"Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Program")
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (trash state side c {:unpreventable true}))
                 (gain state side :credit (* 2 (count targets))))}})
