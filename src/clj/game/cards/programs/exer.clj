(in-ns 'game.cards.programs)

(def card-definition-exer
  {"eXer"
   {:in-play [:rd-access 1]
    :events {:purge {:effect (effect (trash card {:cause :purge}))}}}})
