(in-ns 'game.core)

(def card-definitions-programs-exer
  {"eXer"
   {:in-play [:rd-access 1]
    :events {:purge {:effect (effect (trash card))}} }})
