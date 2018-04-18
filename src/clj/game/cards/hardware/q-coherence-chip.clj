(in-ns 'game.core)

(def card-definitions-hardware-q-coherence-chip
  {"Q-Coherence Chip"
   {:in-play [:memory 1]
    :events (let [e {:req (req (= (last (:zone target)) :program))
                     :effect (effect (trash card)
                                     (system-msg (str "trashes Q-Coherence Chip")))}]
              {:runner-trash e :corp-trash e})}})
