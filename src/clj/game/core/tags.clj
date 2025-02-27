(ns game.core.tags
  (:require
    [game.core.effects :refer [any-effects sum-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [trigger-event trigger-event-simult trigger-event-sync queue-event checkpoint]]
    [game.core.gaining :refer [deduct gain]]
    [game.core.prevention :refer [resolve-tag-prevention]]
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
       (trigger-event state :runner :tags-changed {:new-total new-total
                                                   :old-total old-total
                                                   :is-tagged is-tagged?}))
     changed?)))

(defn- resolve-tag
  "Resolve runner gain tags. Always gives `:base` tags."
  [state side eid {:keys [card n suppress-checkpoint]}]
  (if (pos? n)
    (do (gain state :runner :tag {:base n})
        (toast state :runner (str "Took " (quantify n "tag") "!") "info")
        (update-tag-status state)
        (queue-event state :runner-gain-tag {:side side
                                             :cause-card (select-keys card [:cid :title])
                                             :amount n}))
    (queue-event state :runner-prevents-all-tags {:side side
                                                  :cause-card card}))
  (if suppress-checkpoint
    (effect-completed state nil eid)
    (checkpoint state eid)))

(defn gain-tags
  "Attempts to give the runner n tags, allowing for boosting/prevention effects."
  ([state side eid n] (gain-tags state side eid n nil))
  ([state side eid n {:keys [unpreventable card suppress-checkpoint] :as args}]
   (wait-for
     (resolve-tag-prevention state side n args)
     (resolve-tag state side eid {:suppress-checkpoint suppress-checkpoint
                                  :card card
                                  :n (:remaining async-result)}))))

(defn lose-tags
  "Always removes `:base` tags"
  [state side eid n]
  (if (= n :all)
    (lose-tags state side eid (get-in @state [:runner :tag :base]))
    (do (swap! state update-in [:stats :runner :lose :tag] (fnil + 0) n)
        (deduct state :runner [:tag {:base n}])
        (update-tag-status state)
        (trigger-event-sync state side eid :runner-lose-tag {:amount n
                                                             :side side}))))
