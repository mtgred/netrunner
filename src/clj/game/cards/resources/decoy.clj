(in-ns 'game.cards.resources)

(def card-definition-decoy
  {"Decoy"
   {:interactions {:prevent [{:type #{:tag}
                              :req (req true)}]}
    :abilities [{:msg "avoid 1 tag" :effect (effect (tag-prevent :runner 1) (trash card {:cause :ability-cost}))}]}})
