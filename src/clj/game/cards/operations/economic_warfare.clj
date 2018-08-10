(in-ns 'game.cards.operations)

(def card-definition-economic-warfare
  {"Economic Warfare"
   {:req (req (and (last-turn? state :runner :successful-run)
                   (can-pay? state :runner nil :credit 4)))
    :msg "make the runner lose 4 [Credits]"
    :effect (effect (lose-credits :runner 4))}})
