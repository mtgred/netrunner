(in-ns 'game.core)

(declare can-host?)

(def card-programs-nerve-agent
  {"Nerve Agent"
   {:events
    {:successful-run {:req (req (= target :hq))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:delayed-completion true
                  :req (req (= target :hq))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state side card)))
                                     :prompt "Choose how many additional HQ accesses to make with Nerve Agent"
                                     :choices {:number (req (dec (get-virus-counters state side card)))
                                               :default (req (dec (get-virus-counters state side card)))}
                                     :msg (msg "access " target " additional cards from HQ")
                                     :effect (effect (access-bonus (max 0 target)))}
                                    card nil))}}}})
