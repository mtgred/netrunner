(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-all-nighter
  {"All-nighter"
   {:abilities [{:cost [:click 1] :effect (effect (trash card {:cause :ability-cost}) (gain :click 2))
                 :msg "gain [Click][Click]"}]}})