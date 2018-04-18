(in-ns 'game.core)

(def card-definitions-resources-decoy
  {"Decoy"
   {:prevent {:tag [:all]}
    :abilities [{:msg "avoid 1 tag" :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}]}})
