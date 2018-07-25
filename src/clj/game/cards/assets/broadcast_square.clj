(in-ns 'game.cards.assets)

(def card-definition-broadcast-square
  {"Broadcast Square"
   {:events {:pre-bad-publicity {:async true
                                 :trace {:base 3
                                         :successful {:msg "prevents all bad publicity"
                                                      :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}}}})
