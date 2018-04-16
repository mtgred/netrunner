(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-beth-kilrain-chang
  {"Beth Kilrain-Chang"
   (let [ability {:once :per-turn
                  :label "Gain 1 [Credits], draw 1 card, or gain [Click] (start of turn)"
                  :req (req (:runner-phase-12 @state))
                  :effect (req (let [c (:credit corp)
                                     b (:title card)]
                                 (cond
                                   ;; gain 1 credit
                                   (<= 5 c 9)
                                   (do (gain state side :credit 1)
                                       (system-msg state side (str "uses " b " to gain 1 [Credits]")))
                                   ;; draw 1 card
                                   (<= 10 c 14)
                                   (do (draw state side 1)
                                       (system-msg state side (str "uses " b " to draw 1 card")))
                                   ;; gain 1 click
                                   (<= 15 c)
                                   (do (gain state side :click 1)
                                       (system-msg state side (str "uses " b " to gain [Click]"))))))}]
     {:flags {:drip-economy true}
      :abilities [ability]
      :events {:runner-turn-begins ability}})})