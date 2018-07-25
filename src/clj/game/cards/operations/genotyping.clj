(in-ns 'game.cards.operations)

(def card-definition-genotyping
  {"Genotyping"
   {:async true
    :effect (effect (mill :corp 2)
                    (system-msg "trashes the top 2 cards of R&D")
                    (rfg-and-shuffle-rd-effect eid (first (:play-area corp)) 4 false))}})
