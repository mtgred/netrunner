(in-ns 'game.cards.assets)

(def card-definition-hostile-infrastructure
  {"Hostile Infrastructure"
   {:events {:runner-trash {:async true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg (msg (str "do " (count (filter #(card-is? % :side :corp) targets))
                                           " net damage"))
                            :effect (req (letfn [(do-damage [t]
                                                   (if-not (empty? t)
                                                     (wait-for (damage state side :net 1 {:card card})
                                                               (do-damage (rest t)))
                                                     (effect-completed state side eid)))]
                                           (do-damage (filter #(card-is? % :side :corp) targets))))}}
    :abilities [{:msg "do 1 net damage"
                 :async true
                 :effect (effect (damage eid :net 1 {:card card}))}]}})
