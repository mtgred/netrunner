(in-ns 'game.core)

(def card-definitions-assets-hostile-infrastructure
  {"Hostile Infrastructure"
   {:events {:runner-trash {:delayed-completion true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg (msg (str "do " (count (filter #(card-is? % :side :corp) targets))
                                           " net damage"))
                            :effect (req (letfn [(do-damage [t]
                                                   (if-not (empty? t)
                                                     (when-completed (damage state side :net 1 {:card card})
                                                                     (do-damage (rest t)))
                                                     (effect-completed state side eid card)))]
                                           (do-damage (filter #(card-is? % :side :corp) targets))))}}
    :abilities [{:msg "do 1 net damage"
                 :delayed-completion true
                 :effect (effect (damage eid :net 1 {:card card}))}]}})
