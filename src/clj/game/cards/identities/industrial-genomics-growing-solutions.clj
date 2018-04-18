(in-ns 'game.core)

(def card-definitions-identities-industrial-genomics-growing-solutions
  {"Industrial Genomics: Growing Solutions"
   {:events {:pre-trash {:effect (effect (trash-cost-bonus
                                           (count (filter #(not (:seen %)) (:discard corp)))))}}}})
