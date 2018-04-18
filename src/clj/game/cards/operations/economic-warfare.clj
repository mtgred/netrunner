(in-ns 'game.core)

(def card-definitions-operations-economic-warfare
  {"Economic Warfare"
   {:req (req (and (last-turn? state :runner :successful-run)
                   (can-pay? state :runner nil :credit 4)))
    :msg "make the runner lose 4 [Credits]"
    :effect (effect (lose :runner :credit 4))}})
