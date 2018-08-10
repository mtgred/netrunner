(in-ns 'game.cards.events)

(def card-definition-wanton-destruction
  {"Wanton Destruction"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                               :prompt "How many [Click] do you want to spend?"
                               :choices (req (map str (range 1 (inc (:click runner)))))
                               :effect (req (let [n (str->int target)]
                                              (when (pay state :runner card :click n)
                                                (trash-cards state :corp (take n (shuffle (:hand corp)))))))}} card))}})
