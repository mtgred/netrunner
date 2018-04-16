(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-sacrificial-construct
  {"Sacrificial Construct"
   {:prevent {:trash [:program :hardware]}
    :abilities [{:effect (effect (trash-prevent :program 1) (trash-prevent :hardware 1)
                                 (trash card {:cause :ability-cost}))}]}})