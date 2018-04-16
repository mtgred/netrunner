(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-decoy
  {"Decoy"
   {:prevent {:tag [:all]}
    :abilities [{:msg "avoid 1 tag" :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}]}})