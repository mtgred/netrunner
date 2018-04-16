(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-industrial-genomics-growing-solutions
  {"Industrial Genomics: Growing Solutions"
   {:events {:pre-trash {:effect (effect (trash-cost-bonus
                                           (count (filter #(not (:seen %)) (:discard corp)))))}}}})
