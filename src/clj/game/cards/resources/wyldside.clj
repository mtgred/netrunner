(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-wyldside
  {"Wyldside"
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                      (cons (get-in @state [:runner :identity])
                                                            (all-active-installed state :runner))))))}

    :events {:runner-turn-begins {:effect (req (lose state side :click 1)
                                               (when-not (get-in @state [:per-turn (:cid card)])
                                                 (system-msg state side "uses Wyldside to draw 2 cards and lose [Click]")
                                                 (draw state side 2)))}}
    :abilities [{:msg "draw 2 cards and lose [Click]"
                 :once :per-turn
                 :effect (effect (draw 2))}]}})