(in-ns 'game.core)

(def card-definitions-resources-bio-modeled-network
  {"Bio-Modeled Network"
   {:prevent {:damage [:net]}
    :events {:pre-damage {:req (req (= target :net))
                          :effect (effect (update! (assoc card :dmg-amount (nth targets 2))))}}
    :abilities [{:msg (msg "prevent " (dec (:dmg-amount card)) " net damage")
                 :effect (effect (damage-prevent :net (dec (:dmg-amount card)))
                                 (trash card {:cause :ability-cost}))}]}})
