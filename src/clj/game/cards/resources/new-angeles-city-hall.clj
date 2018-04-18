(in-ns 'game.core)

(def card-definitions-resources-new-angeles-city-hall
  {"New Angeles City Hall"
   {:prevent {:tag [:all]}
    :events {:agenda-stolen {:msg "trash itself" :effect (effect (trash card))}}
    :abilities [{:cost [:credit 2] :msg "avoid 1 tag" :effect (effect (tag-prevent 1))}]}})
