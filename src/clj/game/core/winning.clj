(ns game.core.winning
  (:require
    [game.core.effects :refer [any-effects]]
    [game.core.say :refer [play-sfx system-msg]]
    [game.utils :refer [dissoc-in]]
    [jinteki.utils :refer [other-side]]
    [clj-time.core :as t]))

(defn win
  "Records a win reason for statistics."
  [state side reason]
  (when-not (:winner @state)
    (let [started (get-in @state [:stats :time :started])
          now (t/now)]
      (system-msg state side "wins the game")
      (play-sfx state side "game-end")
      (swap! state assoc-in [:stats :time :ended] now)
      (swap! state assoc-in [:stats :time :elapsed] (t/in-minutes (t/interval started now)))
      (swap! state assoc
             :winner side
             :loser (other-side side)
             :winning-user (get-in @state [side :user :username])
             :losing-user (get-in @state [(other-side side) :user :username])
             :reason reason
             :end-time (java.util.Date.)
             :winning-deck-id (get-in @state [side :deck-id])
             :losing-deck-id (get-in @state [(other-side side) :deck-id])))))

(defn win-decked
  "Records a win via decking the corp."
  [state]
  (system-msg state :corp "is decked")
  (win state :runner "Decked"))

(defn flatline
  "Records a win via dealing damage to the runner."
  [state]
  (when-not (:winner state)
    (system-msg state :runner "is flatlined")
    (win state :corp "Flatline")))

(defn concede
  "Trigger game concede by specified side. Takes a third argument for use with user commands."
  ([state side _] (concede state side))
  ([state side]
   (system-msg state side "concedes")
   (win state (if (= side :corp) :runner :corp) "Concede")))

(defn clear-win
  "Clears the current win condition. Requires both sides to have issued the command"
  [state side]
  (swap! state assoc-in [side :clear-win] true)
  (when (and (-> @state :runner :clear-win) (-> @state :corp :clear-win))
    (system-msg state side "cleared the win condition")
    (swap! state dissoc-in [:runner :clear-win])
    (swap! state dissoc-in [:corp :clear-win])
    (swap! state dissoc :winner :loser :winning-user :losing-user :reason :winning-deck-id :losing-deck-id :end-time)))

(defn check-winner
  [state _]
  (doseq [side [:corp :runner]]
    (when (and (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
               (not (any-effects state side :cannot-win-on-points)))
      (win state side "Agenda"))))
