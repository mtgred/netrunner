(in-ns 'game.cards.upgrades)

(def card-definition-forced-connection
  {"Forced Connection"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 3
                     :successful {:msg "give the Runner 2 tags"
                                  :async true
                                  :effect (effect (gain-tags :corp eid 2))}}}}})
