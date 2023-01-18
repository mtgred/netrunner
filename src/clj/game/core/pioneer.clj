(ns game.core.pioneer
  (:require
   [game.core.card :refer [get-card]]
   [game.core.eid :refer [make-eid]]
   [game.core.engine :refer [pay]]
   [game.core.moving :refer [trash]]
   [game.core.payment :refer [can-pay?]]
   [game.core.revealing :refer [reveal]]
   [game.macros :refer [continue-ability msg req wait-for]]))

(defn pioneer
  [ex]
  {:req (req (and
               ;; this is kind of spaghetti - but it helps enforce the 'no change in gamestate' rule
               ;; still, there's gotta be a better way to precheck these
               (can-pay?
                 state side (assoc eid :source card :source-type :ability) card nil [:click 1])
               ((:req ex) state side eid card targets)))
   :async true
   :msg (msg "spend [Click], reveal " (:title card)
             " and trash it from HQ to resolve it's pioneer ability") ;; name may change
   :effect (req (wait-for (pay state :corp (make-eid state eid) card [:click 1])
                          (wait-for (reveal state :corp (make-eid state eid) [card])
                                    (wait-for (trash state :corp (make-eid state eid)
                                                     (assoc (get-card state card) :seen true))
                                              (continue-ability state :corp ex card nil)))))})
