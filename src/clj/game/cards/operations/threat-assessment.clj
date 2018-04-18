(in-ns 'game.core)

(def card-definitions-operations-threat-assessment
  {"Threat Assessment"
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Select an installed Runner card"
    :choices {:req #(and (= (:side %) "Runner") (installed? %))}
    :delayed-completion true
    :effect (req (let [chosen target]
                   (show-wait-prompt state side "Runner to resolve Threat Assessment")
                   (continue-ability state :runner
                                     {:prompt (str "Add " (:title chosen) " to the top of the Stack or take 2 tags?")
                                      :choices [(str "Move " (:title chosen))
                                                "2 tags"]
                                      :delayed-completion true
                                      :effect (req (clear-wait-prompt state :corp)
                                                   (move state :corp (last (:discard corp)) :rfg)
                                                   (if (.startsWith target "Move")
                                                     (do (system-msg state side (str "chooses to move " (:title chosen) " to the Stack"))
                                                       (move state :runner chosen :deck {:front true})
                                                       (effect-completed state side eid))
                                                     (do (system-msg state side "chooses to take 2 tags")
                                                       (tag-runner state :runner eid 2))))}
                                     card nil)))}})
