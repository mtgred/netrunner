(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-bryan-stinson
  {"Bryan Stinson"
   {:abilities [{:cost [:click 1]
                 :req (req (and (< (:credit runner) 6)
                                (< 0 (count (filter #(and (is-type? % "Operation")
                                                          (has-subtype? % "Transaction")) (:discard corp))))))
                 :label "Play a transaction operation from Archives, ignoring all costs, and remove it from the game"
                 :prompt "Choose a transaction operation to play"
                 :msg (msg "play " (:title target) " from Archives, ignoring all costs, and removes it from the game")
                 :choices (req (cancellable (filter #(and (is-type? % "Operation")
                                                          (has-subtype? % "Transaction")) (:discard corp)) :sorted))
                 :effect (effect (play-instant nil (assoc-in target [:special :rfg-when-trashed] true) {:ignore-cost true})
                                 (move target :rfg))}]}})
