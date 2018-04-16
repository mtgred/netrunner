(in-ns 'game.core)

(def card-operations-genotyping
  {"Genotyping"
   {:delayed-completion true
    :effect (effect (mill :corp 2)
                    (system-msg "trashes the top 2 cards of R&D")
                    (rfg-and-shuffle-rd-effect eid (first (:play-area corp)) 4))}})