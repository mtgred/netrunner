(in-ns 'game.core)

(def card-definitions-assets-broadcast-square
  {"Broadcast Square"
   {:events {:pre-bad-publicity {:delayed-completion true
                                 :trace {:base 3
                                         :msg "prevents all bad publicity"
                                         :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}}})
