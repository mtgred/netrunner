(in-ns 'game.cards.hardware)

(def card-definition-q-coherence-chip
  {"Q-Coherence Chip"
   {:in-play [:memory 1]
    :events (let [e {:req (req (= (last (:zone target)) :program))
                     :effect (effect (trash card)
                                     (system-msg (str "trashes Q-Coherence Chip")))}]
              {:runner-trash e :corp-trash e})}})
