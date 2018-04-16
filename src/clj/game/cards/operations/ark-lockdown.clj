(in-ns 'game.core)

(def card-operations-ark-lockdown
  {"Ark Lockdown"
   {:delayed-completion true
    :req (req (not-empty (:discard runner)))
    :prompt "Name a card to remove all copies in the Heap from the game"
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
    :effect (req (doseq [c (filter #(= (:title target) (:title %)) (:discard runner))]
                   (move state :runner c :rfg))
                 (effect-completed state side eid card))}})