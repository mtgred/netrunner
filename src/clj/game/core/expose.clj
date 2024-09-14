(ns game.core.expose
  (:require
    [game.core.card :refer [rezzed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.eid :refer [effect-completed make-eid make-result]]
    [game.core.engine :refer [resolve-ability trigger-event-sync]]
    [game.core.flags :refer [cards-can-prevent? get-prevent-list]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.to-string :refer [card-str]]
    [game.macros :refer [wait-for]]
    [stringer.core :as s]))

(defn expose-prevent
  [state _ n]
  (swap! state update-in [:expose :expose-prevent] #(+ (or % 0) n)))

(defn- resolve-expose
  [state side eid target]
  (system-msg state side (s/strcat "exposes " (card-str state target {:visible true})))
  (if-let [ability (:on-expose (card-def target))]
    (wait-for (resolve-ability state side ability target nil)
              (trigger-event-sync state side (make-result eid target) :expose target))
    (trigger-event-sync state side (make-result eid target) :expose target)))

(defn expose
  "Exposes the given card."
  ([state side target] (expose state side (make-eid state) target))
  ([state side eid target] (expose state side eid target nil))
  ([state side eid target {:keys [unpreventable]}]
    (swap! state update :expose dissoc :expose-prevent)
    (if (or (rezzed? target)
            (nil? target))
      (effect-completed state side eid) ; cannot expose faceup cards
      (wait-for (trigger-event-sync state side :pre-expose target)
                (let [prevent (get-prevent-list state :corp :expose)]
                  (if (and (not unpreventable)
                           (cards-can-prevent? state :corp prevent :expose))
                    (do (system-msg state :corp "has the option to prevent a card from being exposed")
                        (show-wait-prompt state :runner "Corp to prevent the expose")
                        (show-prompt state :corp nil
                                     (s/strcat "Prevent " (:title target) " from being exposed?") ["Done"]
                                     (fn [_]
                                       (clear-wait-prompt state :runner)
                                       (if (get-in @state [:expose :expose-prevent])
                                         (effect-completed state side (make-result eid false))
                                         (do (system-msg state :corp "will not prevent a card from being exposed")
                                             (resolve-expose state side eid target))))
                                     {:prompt-type :prevent}))
                    (if-not (get-in @state [:expose :expose-prevent])
                      (resolve-expose state side eid target)
                      (effect-completed state side (make-result eid false)))))))))
