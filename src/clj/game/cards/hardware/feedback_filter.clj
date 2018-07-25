(in-ns 'game.cards.hardware)

(def card-definition-feedback-filter
  {"Feedback Filter"
   {:interactions {:prevent [{:type #{:net :brain}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent 1 net damage"
                 :effect (effect (damage-prevent :net 1))}
                {:label "[Trash]: Prevent up to 2 brain damage"
                 :msg "prevent up to 2 brain damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :brain 2))}]}})
