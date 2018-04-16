(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-caldera
  {"Caldera"
   {:prevent {:damage [:net :brain]}
    :abilities [{:cost [:credit 3] :msg "prevent 1 net damage" :effect (effect (damage-prevent :net 1))}
                {:cost [:credit 3] :msg "prevent 1 brain damage" :effect (effect (damage-prevent :brain 1))}]}})