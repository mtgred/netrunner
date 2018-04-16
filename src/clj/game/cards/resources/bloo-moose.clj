(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-bloo-moose
  {"Bloo Moose"
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:req (req (and (:runner-phase-12 @state)
                                (not (seq (get-in @state [:runner :locked :discard])))))
                 :once :per-turn
                 :prompt "Choose a card in the Heap to remove from the game and gain 2 [Credits]"
                 :show-discard true
                 :choices {:req #(and (in-discard? %) (= (:side %) "Runner"))}
                 :msg (msg "remove " (:title target) " from the game and gain 2 [Credits]")
                 :effect (effect (gain :credit 2)
                                 (move target :rfg))}]}})
