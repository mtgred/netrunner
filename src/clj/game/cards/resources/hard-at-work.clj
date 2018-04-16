(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-hard-at-work
  {"Hard at Work"
   (let [ability {:msg "gain 2 [Credits] and lose [Click]"
                  :once :per-turn
                  :effect (effect (lose :click 1) (gain :credit 2))}]
   {:flags {:drip-economy true}
    :events {:runner-turn-begins ability}
    :abilities [ability]})})