(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-liberated-chela
  {"Liberated Chela"
   {:abilities [{:cost [:click 5 :forfeit]
                 :msg "add it to their score area"
                 :effect (req (if (not (empty? (:scored corp)))
                                (do (show-wait-prompt state :runner "Corp to decide whether or not to prevent Liberated Chela")
                                    (resolve-ability
                                      state side
                                      {:prompt (msg "Forfeit an agenda to prevent Liberated Chela from being added to Runner's score area?")
                                       :choices ["Yes" "No"] :player :corp
                                       :effect (final-effect (resolve-ability
                                                               (if (= target "Yes")
                                                                 {:player :corp
                                                                  :prompt "Select an agenda to forfeit"
                                                                  :choices {:req #(in-corp-scored? state side %)}
                                                                  :effect (effect (forfeit target)
                                                                                  (move :runner card :rfg)
                                                                                  (clear-wait-prompt :runner))}
                                                                 {:effect (effect (as-agenda :runner card 2)
                                                                                  (clear-wait-prompt :runner))
                                                                  :msg "add it to their score area as an agenda worth 2 points"})
                                                              card nil))} card nil))
                                (resolve-ability
                                  state side
                                  {:effect (effect (as-agenda :runner card 2))
                                   :msg "add it to their score area as an agenda worth 2 points"} card nil)))}]}})