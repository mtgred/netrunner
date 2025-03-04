(ns game.core.expose
  (:require
    [game.core.card :refer [rezzed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid make-result]]
    [game.core.effects :refer [any-effects]]
    [game.core.engine :refer [checkpoint queue-event register-pending-event resolve-ability trigger-event-sync]]
    [game.core.prevention :refer [resolve-expose-prevention]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.to-string :refer [card-str]]
    [game.utils :refer [enumerate-str]]
    [game.macros :refer [wait-for]]))

(defn resolve-expose
  [state side eid targets {:keys [card] :as args}]
  (if-not (seq targets)
    (effect-completed state side eid)
    (do (system-msg state side (str (if-not card "exposes " (str "uses " (:title card) " to expose ")) (enumerate-str (map #(card-str state % {:visible true}) targets))))
        (doseq [t targets]
          (when-let [ability (:on-expose (card-def t))]
            ;; if it gets rezzed by blackguard or something, the effect shouldn't fizzle
            (register-pending-event state :expose t (assoc ability :condition :installed))))
        (queue-event state :expose {:cards targets})
        (wait-for (checkpoint state side {:duration :expose})
                  (complete-with-result state side eid {:cards targets})))))

(defn expose
  "Exposes the given cards."
  ([state side eid targets] (expose state side eid targets nil))
  ([state side eid targets {:keys [unpreventable] :as args}]
   (let [args (assoc args :card (:source eid))
         targets (filterv #(not (or (rezzed? %)
                                    (nil? %)
                                    (any-effects state side :cannot-be-exposed true? %)))
                          targets)]
     (if (empty? targets)
       (effect-completed state side eid) ;; cannot expose faceup cards
       (wait-for (resolve-expose-prevention state side targets args)
                 (resolve-expose state side eid (:remaining async-result) args))))))
