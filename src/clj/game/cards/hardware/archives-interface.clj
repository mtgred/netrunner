(in-ns 'game.core)

(def card-hardware-archives-interface
  {"Archives Interface"
   {:events
    {:pre-access
     {:delayed-completion true
      :interactive (req true)
      :req (req (and (= target :archives)
                     (not= (:max-access run) 0)
                     (not-empty (:discard corp))))
      :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                   (continue-ability state side
                     {:optional
                      {:delayed-completion true
                       :prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                       :yes-ability
                       {:delayed-completion true
                        :effect (effect (continue-ability
                                          {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                           :choices (req (:discard corp))
                                           :msg (msg "remove " (:title target) " from the game")
                                           :effect (effect (move :corp target :rfg))} card nil))}
                       :no-ability {:effect (req (effect-completed state side eid))}}} card nil))}}}})
