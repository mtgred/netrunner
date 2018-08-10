(in-ns 'game.cards.resources)

(def card-definition-gbahali
  {"Gbahali"
   {:abilities [{:label "[Trash]: Break the last subroutine on the encountered piece of ice"
                 :req (req (and (:run @state) (rezzed? current-ice)))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (system-msg :runner
                                             (str "trashes Gbahali to break the last subroutine on "
                                                  (:title current-ice))))}]}})
