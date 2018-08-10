(in-ns 'game.cards.agendas)

(def card-definition-improved-protein-source
  {"Improved Protein Source"
   {:msg "make the Runner gain 4 [Credits]"
    :effect (effect (gain-credits :runner 4))
    :interactive (req true)
    :stolen {:msg "make the Runner gain 4 [Credits]"
             :effect (effect (gain-credits :runner 4))}}})
