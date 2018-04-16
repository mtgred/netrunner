(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-improved-protein-source
  {"Improved Protein Source"
   {:msg "make the Runner gain 4 [Credits]"
    :effect (effect (gain :runner :credit 4))
    :interactive (req true)
    :stolen {:msg "make the Runner gain 4 [Credits]"
             :effect (effect (gain :runner :credit 4))}}})