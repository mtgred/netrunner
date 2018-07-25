(in-ns 'game.cards.assets)

(def card-definition-elizabeth-mills
  {"Elizabeth Mills"
   {:effect (effect (lose :bad-publicity 1)) :msg "remove 1 bad publicity"
    :abilities [{:cost [:click 1] :label "Trash a location"
                 :msg (msg "trash " (:title target) " and take 1 bad publicity")
                 :choices {:req #(has-subtype? % "Location")}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (trash target)
                                 (gain-bad-publicity :corp 1))}]}})
