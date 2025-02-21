(ns game.core.winning
  (:require
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]
   [cond-plus.core :refer [cond+]]
   [game.core.effects :refer [any-effects sum-effects]]
   [game.core.say :refer [play-sfx system-msg system-say]]
   [game.utils :refer [dissoc-in]]
   [jinteki.utils :refer [other-side]]))

(defn win
  "Records a win reason for statistics."
  [state side reason]
  (when-not (:winner @state)
    (let [started (get-in @state [:stats :time :started])
          now (inst/now)
          duration (duration/to-minutes (duration/between started now))]
      (system-msg state side "wins the game")
      (play-sfx state side "game-end")
      (swap! state (fn [state]
                     (-> state
                         (assoc-in [:stats :time :ended] now)
                         (assoc-in [:stats :time :elapsed] duration)
                         (assoc
                           :winner side
                           :loser (other-side side)
                           :winning-user (get-in state [side :user :username])
                           :losing-user (get-in state [(other-side side) :user :username])
                           :reason reason
                           :end-time now
                           :winning-deck-id (get-in state [side :deck-id])
                           :losing-deck-id (get-in state [(other-side side) :deck-id])))))
      true)))

(defn tie
  "Records a tie reason for statistics."
  [state reason]
  (when-not (:winner @state)
    (let [started (get-in @state [:stats :time :started])
          now (inst/now)
          duration (duration/to-minutes (duration/between started now))]
      (system-say state nil "The game is a tie!")
      (play-sfx state nil "game-end")
      (swap! state (fn [state]
                     (-> state
                         (assoc-in [:stats :time :ended] now)
                         (assoc-in [:stats :time :elapsed] duration)
                         (assoc :reason reason
                                :end-time now))))
      true)))

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

(defn agenda-points-required-to-win
  [state side]
  (+ (get-in @state [side :agenda-point-req])
     (sum-effects state side :agenda-point-req)))

(defn side-win
  [state side]
  (<= (agenda-points-required-to-win state side) (get-in @state [side :agenda-point])))

(defn check-win-by-agenda
  ([state] (check-win-by-agenda state nil))
  ([state _]
   (let [corp-win (side-win state :corp)
         blocked-corp (any-effects state :corp :cannot-win-on-points)
         runner-win (side-win state :runner)
         blocked-runner (any-effects state :runner :cannot-win-on-points)]
     (cond+
       [(and corp-win (not blocked-corp)
             runner-win (not blocked-runner))
        (tie state "Tie")]
       [(and corp-win (not blocked-corp))
        (win state :corp "Agenda")]
       [(and runner-win (not blocked-runner))
        (win state :runner "Agenda")]))))
