(in-ns 'game.cards.events)

(def card-definition-vamp
  {"Vamp"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:async true
                               :prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose-credits :corp target)
                                               (gain-tags eid 1))}} card))}})
