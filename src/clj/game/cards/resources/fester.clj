(in-ns 'game.core)

(def card-definitions-resources-fester
  {"Fester"
   {:events {:purge {:msg "force the Corp to lose 2 [Credits] if able"
                     :effect (effect (pay :corp card :credit 2))}}}})
