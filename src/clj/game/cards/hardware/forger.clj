(in-ns 'game.core)

(def card-hardware-forger
  {"Forger"
   {:prevent {:tag [:all]}
    :in-play [:link 1]
    :abilities [{:msg "avoid 1 tag" :label "[Trash]: Avoid 1 tag"
                 :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Trash]: Remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose :tag 1))}]}})