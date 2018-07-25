(in-ns 'game.cards.resources)

(def card-definition-liberated-chela
  {"Liberated Chela"
   {:abilities [{:cost [:click 5 :forfeit]
                 :msg "add it to their score area"
                 :async true
                 :effect (req (if (not (empty? (:scored corp)))
                                (do (show-wait-prompt state :runner "Corp to decide whether or not to prevent Liberated Chela")
                                    (continue-ability
                                      state side
                                      {:prompt (msg "Forfeit an agenda to prevent Liberated Chela from being added to Runner's score area?")
                                       :choices ["Yes" "No"]
                                       :player :corp
                                       :async true
                                       :effect (effect (continue-ability
                                                         (if (= target "Yes")
                                                           {:player :corp
                                                            :prompt "Select an agenda to forfeit"
                                                            :choices {:req #(in-corp-scored? state side %)}
                                                            :effect (effect (forfeit target)
                                                                            (move :runner card :rfg)
                                                                            (clear-wait-prompt :runner))}
                                                           {:async true
                                                            :effect (req (clear-wait-prompt state :runner)
                                                                         (as-agenda state :runner eid card 2))
                                                            :msg "add it to their score area as an agenda worth 2 points"})
                                                         card nil))} card nil))
                                (continue-ability
                                  state side
                                  {:async true
                                   :effect (req (as-agenda state :runner eid card 2))
                                   :msg "add it to their score area as an agenda worth 2 points"} card nil)))}]}})
