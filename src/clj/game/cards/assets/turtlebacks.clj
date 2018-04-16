(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-turtlebacks
  {"Turtlebacks"
   {:events {:server-created {:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}})
