(in-ns 'game.cards.resources)

(def card-definition-new-angeles-city-hall
  {"New Angeles City Hall"
   {:interactions {:prevent [{:type #{:tag}
                              :req (req true)}]}
    :events {:agenda-stolen {:msg "trash itself"
                             :effect (effect (trash card))}}
    :abilities [{:cost [:credit 2]
                 :msg "avoid 1 tag"
                 :effect (effect (tag-prevent :runner 1))}]}})
