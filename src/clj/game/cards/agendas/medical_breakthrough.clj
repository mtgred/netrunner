(in-ns 'game.cards.agendas)

(def card-definition-medical-breakthrough
  {"Medical Breakthrough"
   {:silent (req true)
    :effect (effect (update-all-advancement-costs))
    :stolen {:effect (effect (update-all-advancement-costs))}
    :advancement-cost-bonus (req (- (count (filter #(= (:title %) "Medical Breakthrough")
                                                   (concat (:scored corp) (:scored runner))))))}})
