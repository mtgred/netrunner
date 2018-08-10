(in-ns 'game.cards.programs)

(def card-definition-disrupter
  {"Disrupter"
   {:events
    {:pre-init-trace
     {:async true
      :effect (effect (show-wait-prompt :corp "Runner to use Disrupter")
                      (continue-ability :runner
                        {:optional
                         {:prompt "Use Disrupter's ability?"
                          :yes-ability
                          {:effect (req (trash state side card {:cause :ability-cost})
                                        (swap! state assoc-in [:trace :force-base] 0))}
                          :end-effect (effect (clear-wait-prompt :corp))}}
                        card nil))}}}})
