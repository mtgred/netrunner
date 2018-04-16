(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-forced-connection
  {"Forced Connection"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 3
                     :msg "give the Runner 2 tags"
                     :delayed-completion true
                     :effect (effect (tag-runner :runner eid 2))}}}})