(in-ns 'game.cards.events)

(def card-definition-syn-attack
  {"SYN Attack"
   {:effect (req (if (< (count (:hand corp)) 2)
                   (draw state :corp 4)
                   (do (show-wait-prompt state :runner "Corp to choose an option for SYN Attack")
                       (resolve-ability state :corp
                         {:prompt "Discard 2 cards or draw 4 cards?"
                          :choices ["Discard 2" "Draw 4"]
                          :effect (req (if (= target "Draw 4")
                                         (do (draw state :corp 4)
                                             (system-msg state :corp (str "draws 4 cards from SYN Attack"))
                                             (clear-wait-prompt state :runner))
                                         (resolve-ability state :corp
                                           {:prompt "Choose 2 cards to discard"
                                            :choices {:max 2 :req #(and (in-hand? %) (= (:side %) "Corp"))}
                                            :effect (effect (trash-cards :corp targets)
                                                            (system-msg :corp (str "discards 2 cards from SYN Attack"))
                                                            (clear-wait-prompt :runner))}
                                          card nil)))}
                        card nil))))}})
