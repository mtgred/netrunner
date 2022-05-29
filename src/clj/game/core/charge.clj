(ns game.core.charge
  (:require
   [game.core.board :refer [all-installed]]
   [game.core.card :refer :all]
   [game.core.eid :refer [effect-completed]]
   [game.core.say :refer [system-msg]]
   [game.macros :refer [req msg effect]]
   [game.core.props :refer [add-counter]]))

(defn can-charge
  ([state side]
   (let [cards (all-installed state side)]
     (some #(can-charge state side %) cards)))
  ([state side card]
   "A card can be charged if it has at least one power counter"
   (pos? (get-counters (get-card state card) :power))))

(defn charge-card
  ([state side eid target]
   "Charge: place a power counter on a card that has at least one power counter"
   (charge-card state side eid target 1))
  ([state side eid target count]
   "Charge X: place X power counters on a card that has at least one power counter"
   (if (can-charge state side target)
     (do (add-counter state side eid target :power count {:placed true})
         (effect-completed state side eid))
     (effect-completed state side eid))))

(defn charge-ability
  ;;([state side card] (charge state side nil card))
  ([state side eid card]
   "Creates a charge prompt (if there is a valid target) to charge a card once"
   (charge-ability state side eid card 1))
  ([state side eid card n]
   "Creates a charge prompt (if there is a valid target) to charge a card n times"
   (if (can-charge state side)
     {:waiting-prompt (format "%s to charge a card" (if (= :runner side) "Runner" "Corp"))
      :prompt (str "Select a card to charge")
      :choices {:card #(can-charge state side %)}
      :async true
      :msg (msg "charge " (:title target) (when (> n 1) (str n " times")))
      :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to charge a card"))
                             (effect-completed eid))
      :effect (req (charge-card state side eid target n))}
     (effect-completed state side eid))))
