(in-ns 'game.cards.hardware)

(def card-definition-forger
  {"Forger"
   {:interactions {:prevent [{:type #{:tag}
                              :req (req true)}]}
    :in-play [:link 1]
    :abilities [{:msg "avoid 1 tag" :label "[Trash]: Avoid 1 tag"
                 :effect (effect (tag-prevent :runner 1) (trash card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Trash]: Remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose-tags 1))}]}})
