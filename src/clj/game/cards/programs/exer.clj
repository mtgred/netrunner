(in-ns 'game.core)

(declare can-host?)

(def card-programs-exer
  {"eXer"
   {:in-play [:rd-access 1]
    :events {:purge {:effect (effect (trash card))}} }})
