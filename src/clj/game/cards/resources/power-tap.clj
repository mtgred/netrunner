(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-power-tap
  {"Power Tap"
   {:events {:trace {:msg "gain 1 [Credits]" :effect (effect (gain :runner :credit 1))}}}})