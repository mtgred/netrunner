(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-crash-space
  {"Crash Space"
   {:prevent {:damage [:meat]}
    :recurring 2
    :abilities [{:label "Trash to prevent up to 3 meat damage"
                 :msg "prevent up to 3 meat damage"
                 :effect (effect (trash card {:cause :ability-cost}) (damage-prevent :meat 3))}]}})
