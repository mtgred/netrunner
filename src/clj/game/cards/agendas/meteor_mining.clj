(in-ns 'game.cards.agendas)

(def card-definition-meteor-mining
  {"Meteor Mining"
   {:interactive (req true)
    :async true
    :prompt "Use Meteor Mining?"
    :choices (req (if (< (:tag runner) 2)
                    ["Gain 7 [Credits]" "No action"]
                    ["Gain 7 [Credits]" "Do 7 meat damage" "No action"]))
    :effect (req (case target
                   "Gain 7 [Credits]"
                   (do (gain-credits state side 7)
                       (system-msg state side "uses Meteor Mining to gain 7 [Credits]")
                       (effect-completed state side eid))
                   "Do 7 meat damage"
                   (do (damage state side eid :meat 7 {:card card})
                       (system-msg state side "uses Meteor Mining do 7 meat damage"))
                   "No action"
                   (do (system-msg state side "does not use Meteor Mining")
                       (effect-completed state side eid))))}})
