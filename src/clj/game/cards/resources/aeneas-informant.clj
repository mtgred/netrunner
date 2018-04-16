(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-aeneas-informant
  {"Aeneas Informant"
   {:events {:no-trash {:req (req (and (:trash target) (not= (first (:zone target)) :discard)))
                        :optional {:prompt (msg "Use Aeneas Informant?")
                                   :yes-ability {:msg (msg (str "gain 1 [Credits] and reveal " (:title target)))
                                                 :effect (effect (gain :credit 1))}}}}}})
