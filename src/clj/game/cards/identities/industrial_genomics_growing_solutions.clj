(in-ns 'game.cards.identities)

(def card-definition-industrial-genomics-growing-solutions
  {"Industrial Genomics: Growing Solutions"
   {:events {:pre-trash {:effect (effect (trash-cost-bonus
                                           (count (remove #(:seen %) (:discard corp)))))}}}})
