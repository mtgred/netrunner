(in-ns 'game.core)

(declare run-event)

(def card-events-vamp
  {"Vamp"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:delayed-completion true
                               :prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose :corp :credit target)
                                               (tag-runner eid 1))}} card))}})