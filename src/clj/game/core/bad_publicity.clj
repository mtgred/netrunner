(ns game.core.bad-publicity
  (:require
    [game.core.eid :refer [effect-completed make-eid make-result]]
    [game.core.engine :refer [trigger-event trigger-event-sync]]
    [game.core.flags :refer [cards-can-prevent? get-prevent-list]]
    [game.core.gaining :refer [gain lose]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.toasts :refer [toast]]
    [game.macros :refer [wait-for]]))

(defn bad-publicity-prevent
  [state side n]
  (swap! state update-in [:bad-publicity :bad-publicity-prevent] (fnil #(+ % n) 0))
  (trigger-event state side (if (= side :corp) :corp-prevent :runner-prevent) {:type :bad-publicity
                                                                               :amount n}))

(defn- resolve-bad-publicity
  [state side eid n]
  (if (pos? n)
    (do (gain state :corp :bad-publicity n)
        (toast state :corp (str "Took " n " bad publicity!") "info")
        (trigger-event-sync state side (make-result eid n) :corp-gain-bad-publicity {:amount n}))
    (effect-completed state side eid)))

(defn- bad-publicity-count
  "Calculates the number of bad publicity to give, taking into account prevention and boosting effects."
  [state _ n {:keys [unpreventable unboostable]}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:bad-publicity :bad-publicity-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:bad-publicity :bad-publicity-prevent])) 0))
      (max 0)))

(defn gain-bad-publicity
  "Attempts to give the corp n bad publicity, allowing for boosting/prevention effects."
  ([state side n] (gain-bad-publicity state side (make-eid state) n nil))
  ([state side eid n] (gain-bad-publicity state side eid n nil))
  ([state side eid n {:keys [unpreventable card] :as args}]
   (swap! state update-in [:bad-publicity] dissoc :bad-publicity-bonus :bad-publicity-prevent)
   (wait-for (trigger-event-sync state side :pre-bad-publicity card)
             (let [n (bad-publicity-count state side n args)
                   prevent (get-prevent-list state :corp :bad-publicity)]
               (if (and (pos? n)
                        (not unpreventable)
                        (cards-can-prevent? state :corp prevent :bad-publicity))
                 (do (system-msg state :corp "has the option to avoid bad publicity")
                     (show-wait-prompt state :runner "Corp to prevent bad publicity")
                     (swap! state assoc-in [:prevent :current] :bad-publicity)
                     (show-prompt
                       state :corp nil
                       (str "Avoid " (when (< 1 n) "any of the ") n " bad publicity?") ["Done"]
                       (fn [_]
                         (let [prevent (get-in @state [:bad-publicity :bad-publicity-prevent])]
                           (system-msg state :corp
                                       (if prevent
                                         (str "avoids "
                                              (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                              " bad publicity")
                                         "will not avoid bad publicity"))
                           (clear-wait-prompt state :runner)
                           (resolve-bad-publicity state side eid (max 0 (- n (or prevent 0))))))))
                 (resolve-bad-publicity state side eid n))))))

(defn lose-bad-publicity
  ([state side n] (lose-bad-publicity state side (make-eid state) n))
  ([state side eid n]
   (if (= n :all)
     (lose-bad-publicity state side eid (get-in @state [:corp :bad-publicity :base]))
     (do (lose state :corp :bad-publicity n)
         (trigger-event-sync state side eid :corp-lose-bad-publicity {:amount n
                                                                      :side side})))))
