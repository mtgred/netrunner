(ns game.core.charge
  (:require
   [game.core.board :refer [all-installed]]
   [game.core.card :refer :all]
   [game.core.eid :refer [effect-completed]]
   [game.core.say :refer [system-msg]]
   [game.macros :refer [req msg effect]]
   [game.core.props :refer [add-counter]]))

(defn can-charge
  "A card can be charged if it has at least one power counter"
  ([state side]
   (let [cards (all-installed state side)]
     (some #(can-charge state side %) cards)))
  ([state side card]
   (pos? (get-counters (get-card state card) :power))))

(defn charge-card
  "Charge: place a power counter on a card that has at least one power counter"
  ([state side eid target]
   (charge-card state side eid target 1))
  ([state side eid target count]
   (if (can-charge state side target)
     (add-counter state side eid target :power count {:placed true})
     (effect-completed state side eid))))

(defn charge-ability
  "Creates a charge prompt (if there is a valid target) to charge a card once"
  ([state side]
   (charge-ability state side 1))
  ([state side n]
   (when (can-charge state side)
     {:waiting-prompt true
      :prompt "Choose an installed card"
      :choices {:card #(can-charge state side %)}
      :async true
      :msg (msg "charge " (:title target) (when (> n 1) (str n " times")))
      :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to charge a card"))
                             (effect-completed eid))
      :effect (req (charge-card state side eid target n))})))
