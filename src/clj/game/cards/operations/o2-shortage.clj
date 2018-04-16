(in-ns 'game.core)

(def card-operations-o2-shortage
  {"O₂ Shortage"
   {:delayed-completion true
    :effect (req (if (empty? (:hand runner))
                   (do (gain state :corp :click 2)
                       (system-msg state side (str "uses O₂ Shortage to gain [Click][Click]"))
                       (effect-completed state side eid))
                   (do (show-wait-prompt state :corp "Runner to decide whether or not to trash a card from their Grip")
                       (continue-ability state side
                         {:optional
                          {:prompt "Trash 1 random card from your Grip?"
                           :player :runner
                           :yes-ability {:effect (effect (trash-cards :runner (take 1 (shuffle (:hand runner))))
                                                         (clear-wait-prompt :corp))}
                           :no-ability {:msg "gain [Click][Click]"
                                        :effect (effect (gain :corp :click 2)
                                                        (clear-wait-prompt :corp))}}}
                        card nil))))}})