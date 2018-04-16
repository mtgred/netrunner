(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-meteor-mining
  {"Meteor Mining"
   {:interactive (req true)
    :delayed-completion true
    :prompt "Use Meteor Mining?"
    :choices (req (if (< (:tag runner) 2)
                    ["Gain 7 [Credits]" "No action"]
                    ["Gain 7 [Credits]" "Do 7 meat damage" "No action"]))
    :effect (req (case target
                   "Gain 7 [Credits]"
                   (do (gain state side :credit 7)
                       (system-msg state side "uses Meteor Mining to gain 7 [Credits]")
                       (effect-completed state side eid))
                   "Do 7 meat damage"
                   (do (damage state side eid :meat 7 {:card card})
                       (system-msg state side "uses Meteor Mining do 7 meat damage"))
                   "No action"
                   (do (system-msg state side "does not use Meteor Mining")
                       (effect-completed state side eid))))}})