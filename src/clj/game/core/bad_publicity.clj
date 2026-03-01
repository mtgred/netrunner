(ns game.core.bad-publicity
  (:require
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.effects :refer [any-effects]]
   [game.core.engine :refer [queue-event checkpoint trigger-event-sync]]
   [game.core.gaining :refer [gain lose]]
   [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
   [game.core.prevention :refer [resolve-bad-pub-prevention]]
   [game.core.say :refer [system-msg]]
   [game.core.toasts :refer [toast]]
   [game.macros :refer [wait-for]]))

(defn bad-publicity-available
  "The amount of bad publicity available for this run"
  ([state side]
   (if (= side :runner)
     (get-in @state [:run :bad-publicity-available] 0)
     0)))

(defn- resolve-bad-publicity
  [state side eid n {:keys [suppress-checkpoint] :as args}]
  (if (pos? n)
    (do (gain state :corp :bad-publicity n)
        (toast state :corp (str "Took " n " bad publicity!") "info")
        (queue-event state :corp-gain-bad-publicity {:amount n})
        (if suppress-checkpoint
          (effect-completed state side eid)
          (checkpoint state eid)))
    (effect-completed state side eid)))

(defn gain-bad-publicity
  "Attempts to give the corp n bad publicity, allowing for boosting/prevention effects."
  ([state side n] (gain-bad-publicity state side (make-eid state) n nil))
  ([state side eid n] (gain-bad-publicity state side eid n nil))
  ([state side eid n {:keys [unpreventable card] :as args}]
   (wait-for (resolve-bad-pub-prevention state side n args)
             (resolve-bad-publicity state side eid (:remaining async-result) args))))

(defn lose-bad-publicity
  ([state side n] (lose-bad-publicity state side (make-eid state) n))
  ([state side eid n] (lose-bad-publicity state side eid n nil))
  ([state side eid n {:keys [no-event]}]
   (if (= n :all)
     (lose-bad-publicity state side eid (get-in @state [:corp :bad-publicity :base]))
     (let [n (min n (get-in @state [:corp :bad-publicity :base]))]
       (do (lose state :corp :bad-publicity n)
           (if no-event
             (effect-completed state side eid)
             (trigger-event-sync state side eid :corp-lose-bad-publicity {:amount n
                                                                          :side side})))))))

(defn spend-bad-publicity
  "Spend a bad pub"
  [state side amt]
  (when (and (= side :runner) (pos? (bad-publicity-available state side)))
    (swap! state update-in [:run :bad-publicity-available] #(- % amt))))
