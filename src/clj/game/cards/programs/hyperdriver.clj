(in-ns 'game.core)

(declare can-host?)

(def card-programs-hyperdriver
  {"Hyperdriver"
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:label "Remove Hyperdriver from the game to gain [Click] [Click] [Click]"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (move card :rfg) (gain :click 3))
                 :msg "gain [Click] [Click] [Click]"}]}})