(ns game.core.drawing
  (:require
    [game.core.eid :refer [effect-completed make-eid make-result]]
    [game.core.engine :refer [checkpoint queue-event trigger-event trigger-event-sync]]
    [game.core.events :refer [first-event?]]
    [game.core.flags :refer [prevent-draw]]
    [game.core.moving :refer [move]]
    [game.core.say :refer [system-msg]]
    [game.core.winning :refer [win-decked]]
    [game.macros :refer [req wait-for]]
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

(defn first-time-draw-bonus
  [side n]
  (let [event (keyword (str "pre-" (name side) "-draw"))]
    {:event event
     :msg "draw 1 additional card"
     ;; The req catches draw events that happened before the card was installed
     :req (req (first-event? state side event))
     :once :per-turn
     :effect (req (draw-bonus state side n))}))

(defn draw
  "Draw n cards from :deck to :hand."
  ([state side eid n] (draw state side eid n nil))
  ([state side eid n {:keys [suppress-event no-update-draw-stats]}]
   (if (zero? n)
     (effect-completed state side eid)
     (do
       (trigger-event state side (if (= side :corp) :pre-corp-draw :pre-runner-draw) n)
       (let [n (+ n (get-in @state [:bonus :draw] 0))
             draws-wanted n
             active-player (get-in @state [:active-player])
             draws-after-prevent (if (and (= side active-player) (get-in @state [active-player :register :max-draw]))
                                   (min n (remaining-draws state side))
                                   n)
             deck-count (count (get-in @state [side :deck]))]
         (swap! state update :bonus dissoc :draw);; clear bonus draws
         (when (and (= side :corp) (< deck-count draws-after-prevent))
           (win-decked state))
         (when (< draws-after-prevent draws-wanted)
           (let [prevented (- draws-wanted draws-after-prevent)]
             (system-msg state (other-side side)
                         (str "prevents " (quantify prevented "card") " from being drawn"))))
         (if (or (and (= side active-player)
                      (get-in @state [side :register :cannot-draw]))
                 (not (pos? draws-after-prevent))
                 (not (pos? deck-count)))
           (effect-completed state side eid)
           (let [to-draw (take draws-after-prevent (get-in @state [side :deck]))
                 drawn (mapv #(move state side % :hand) to-draw)
                 drawn-count (count drawn)]
             (swap! state update-in [side :register :drawn-this-turn] (fnil #(+ % drawn-count) 0))
             (if (not no-update-draw-stats)
                 (swap! state update-in [:stats side :gain :card] (fnil + 0) n))
             (if suppress-event
               (effect-completed state side eid)
               (let [draw-event (if (= side :corp) :corp-draw :runner-draw)]
                 (swap! state update-in [side :register :currently-drawing] conj drawn)
                 (queue-event state draw-event {:cards drawn
                                                :count drawn-count})
                 (wait-for
                  (checkpoint state nil (make-eid state eid) nil)
                  (wait-for (trigger-event-sync state side (make-eid state eid) (if (= side :corp) :post-corp-draw :post-runner-draw) drawn-count)
                            (let [eid (make-result eid (-> @state side :register :currently-drawing (peek)))]
                              (swap! state update-in [side :register :currently-drawing] pop)
                              (effect-completed state side eid))))))
             (when (safe-zero? (remaining-draws state side))
               (prevent-draw state side)))))))))
