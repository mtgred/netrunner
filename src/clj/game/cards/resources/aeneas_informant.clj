(in-ns 'game.cards.resources)

(def card-definition-aeneas-informant
  {"Aeneas Informant"
   {:events {:no-trash {:req (req (and (:trash target)
                                       (not= (first (:zone target)) :discard)))
                        :optional {:prompt (msg "Use Aeneas Informant?")
                                   :yes-ability {:msg (msg (str "gain 1 [Credits]"
                                                                (when-not (installed? target)
                                                                  (str " and reveal "  (:title target)))))
                                                 :effect (effect (gain-credits 1))}}}}}})
