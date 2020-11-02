(ns game.core.drawing
  (:require
    [game.core.eid :refer [effect-completed make-eid make-result]]
    [game.core.engine :refer [trigger-event-simult trigger-event-sync]]
    [game.core.flags :refer [prevent-draw]]
    [game.core.moving :refer [move]]
    [game.core.say :refer [system-msg]]
    [game.core.winning :refer [win-decked]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [quantify safe-zero?]]
    [jinteki.utils :refer [other-side]]))

(defn max-draw
  "Put an upper limit on the number of cards that can be drawn in this turn."
  [state side n]
  (swap! state assoc-in [side :register :max-draw] n))

(defn remaining-draws
  "Calculate remaining number of cards that can be drawn this turn if a maximum exists"
  [state side]
  (when-let [max-draw (get-in @state [side :register :max-draw])]
    (let [drawn-this-turn (get-in @state [side :register :drawn-this-turn] 0)]
      (max (- max-draw drawn-this-turn) 0))))

(defn draw-bonus
  "Registers a bonus of n draws to the next draw (Daily Business Show)"
  [state _ n]
  (swap! state update-in [:bonus :draw] (fnil #(+ % n) 0)))

(defn draw
  "Draw n cards from :deck to :hand."
  ([state side] (draw state side (make-eid state) 1 nil))
  ([state side n] (draw state side (make-eid state) n nil))
  ([state side n args] (draw state side (make-eid state) n args))
  ([state side eid n {:keys [suppress-event]}]
   (swap! state update-in [side :register] dissoc :most-recent-drawn) ;clear the most recent draw in case draw prevented
   (wait-for (trigger-event-simult state side (if (= side :corp) :pre-corp-draw :pre-runner-draw) nil n)
             (let [active-player (get-in @state [:active-player])
                   n (+ n (get-in @state [:bonus :draw] 0))
                   draws-wanted n
                   draws-after-prevent (if (and (= side active-player) (get-in @state [active-player :register :max-draw]))
                                         (min n (remaining-draws state side))
                                         n)
                   deck-count (count (get-in @state [side :deck]))]
               (when (and (= side :corp) (> draws-after-prevent deck-count))
                 (win-decked state))
               (if (or (and (= side active-player)
                            (get-in @state [side :register :cannot-draw]))
                       (not (pos? draws-after-prevent))
                       (not (pos? deck-count)))
                 (effect-completed state side eid)
                 (let [to-draw (take draws-after-prevent (get-in @state [side :deck]))
                       drawn (doall (for [card to-draw] (move state side card :hand)))]
                   (swap! state assoc-in [side :register :most-recent-drawn] drawn)
                   (swap! state update-in [side :register :drawn-this-turn] (fnil #(+ % draws-after-prevent) 0))
                   (swap! state update-in [:stats side :gain :card] (fnil + 0) n)
                   (swap! state update-in [:bonus] dissoc :draw)
                   (if (and (not suppress-event) (pos? deck-count))
                     (wait-for
                       (trigger-event-sync state side (if (= side :corp) :corp-draw :runner-draw) draws-after-prevent)
                       (let [eid (make-result eid drawn)]
                         (trigger-event-sync state side eid (if (= side :corp) :post-corp-draw :post-runner-draw) draws-after-prevent)))
                     (effect-completed state side eid))
                   (when (safe-zero? (remaining-draws state side))
                     (prevent-draw state side))))
               (when (< draws-after-prevent draws-wanted)
                 (let [prevented (- draws-wanted draws-after-prevent)]
                   (system-msg state (other-side side) (str "prevents "
                                                            (quantify prevented "card")
                                                            " from being drawn"))))))))

