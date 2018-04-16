(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-broadcast-square
  {"Broadcast Square"
   {:events {:pre-bad-publicity {:delayed-completion true
                                 :trace {:base 3
                                         :msg "prevents all bad publicity"
                                         :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}}})
