(ns game.core.tags
  (:require
    [game.core.effects :refer [any-effects sum-effects]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [trigger-event trigger-event-simult trigger-event-sync]]
    [game.core.flags :refer [cards-can-prevent? get-prevent-list]]
    [game.core.gaining :refer [deduct gain]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.toasts :refer [toast]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [pluralize quantify]]))

(defn sum-tag-effects
  [state]
  (+ (or (get-in @state [:runner :tag :base]) 0)
     (sum-effects state :runner :user-tags)
     (sum-effects state :runner :tags)))

(defn update-tag-status
  ([state] (update-tag-status state nil))
  ([state _]
   (let [old-total (get-in @state [:runner :tag :total])
         new-total (sum-tag-effects state)
         is-tagged? (or (any-effects state :runner :is-tagged)
                        (pos? new-total))
         old-tags (select-keys (get-in @state [:runner :tag]) [:total :is-tagged])
         new-tags {:total new-total
                   :is-tagged is-tagged?}
         changed? (not= old-tags new-tags)]
     (when changed?
       (swap! state update-in [:runner :tag] merge new-tags)
       (trigger-event state :runner :tags-changed new-total old-total is-tagged?))
     changed?)))

(defn tag-prevent
  [state side eid n]
  (swap! state update-in [:tag :tag-prevent] (fnil #(+ % n) 0))
  (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) (list :tag n)))

(defn- number-of-tags-to-gain
  "Calculates the number of tags to give, taking into account prevention and boosting effects."
  [state _ n {:keys [unpreventable unboostable]}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:tag :tag-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:tag :tag-prevent])) 0))
      (max 0)))

(defn- resolve-tag
  "Resolve runner gain tags. Always gives `:base` tags."
  [state side eid n]
  (if (pos? n)
    (do (gain state :runner :tag {:base n})
        (toast state :runner (str "Took " (quantify n "tag") "!") "info")
        (update-tag-status state)
        (trigger-event-simult state side eid :runner-gain-tag nil nil n))
    (effect-completed state side eid)))

(defn gain-tags
  "Attempts to give the runner n tags, allowing for boosting/prevention effects."
  ([state side eid n] (gain-tags state side eid n nil))
  ([state side eid n {:keys [unpreventable card] :as args}]
   (swap! state update-in [:tag] dissoc :tag-bonus :tag-prevent)
   (wait-for (trigger-event-simult state side :pre-tag nil card)
             (let [n (number-of-tags-to-gain state side n args)
                   prevent (get-prevent-list state :runner :tag)]
               (if (and (pos? n)
                        (not unpreventable)
                        (cards-can-prevent? state :runner prevent :tag))
                 (do (system-msg state :runner "has the option to avoid tags")
                     (show-wait-prompt state :corp "Runner to prevent tags")
                     (swap! state assoc-in [:prevent :current] :tag)
                     (show-prompt
                       state :runner nil
                       (str "Avoid " (when (< 1 n) "any of the ") (quantify n "tag") "?") ["Done"]
                       (fn [_]
                         (let [prevent (get-in @state [:tag :tag-prevent])
                               prevent-msg (if prevent
                                             (str "avoids "
                                                  (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                                  (pluralize prevent "tag"))
                                             "will not avoid tags")]
                           (system-msg state :runner prevent-msg)
                           (clear-wait-prompt state :corp)
                           (resolve-tag state side eid (max 0 (- n (or prevent 0))))))))
                 (resolve-tag state side eid n))))))

(defn lose-tags
  "Always removes `:base` tags"
  [state side eid n]
  (if (= n :all)
    (lose-tags state side eid (get-in @state [:runner :tag :base]))
    (do (swap! state update-in [:stats :runner :lose :tag] (fnil + 0) n)
        (deduct state :runner [:tag {:base n}])
        (update-tag-status state)
        (trigger-event-sync state side eid :runner-lose-tag n side))))
