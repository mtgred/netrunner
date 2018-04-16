(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-first-responders
  {"First Responders"
   {:abilities [{:cost [:credit 2]
                 :req (req (some #(= (:side %) "Corp") (map second (turn-events state :runner :damage))))
                 :msg "draw 1 card"
                 :effect (effect (draw))}]}})