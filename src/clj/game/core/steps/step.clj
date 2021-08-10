(ns game.core.steps.step
  (:require
   [clj-uuid :as uuid]
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]))

(defmulti validate-step
  "Expects the step to be wrapped in a volatile."
  (fn [step] (:type @step)))

(def BaseStepSchema
  [:map {:closed true}
   [:complete? boolean?]
   [:continue [:=> [:cat :map :map] :boolean]]
   [:on-card-clicked [:=> [:cat :map] :boolean]]
   [:on-prompt-clicked [:=> [:cat :map] :boolean]]
   [:type [:qualified-keyword {:namespace :step}]]
   [:uuid uuid?]])

(def SimpleStepSchema
  (mu/merge
    BaseStepSchema
    [:map
     [:continue [:=> [:cat :map] :boolean]]]))

(def validate-simple-step (m/validator SimpleStepSchema))
(def explain-simple-step (m/explainer SimpleStepSchema))

(defmethod validate-step :step/simple
  [step]
  (if (validate-simple-step @step)
    step
    (let [explained-error (explain-simple-step @step)]
      (throw (ex-info (str "Simple step isn't valid: " (pr-str (me/humanize explained-error)))
                      explained-error)))))

(defn default-on-card-clicked [_step])
(defn default-on-prompt-clicked [_step])

(defn make-base-step
  [step-type continue]
  (let [step {:type step-type
              :complete? false
              :continue continue
              :on-card-clicked default-on-card-clicked
              :on-prompt-clicked default-on-prompt-clicked
              :uuid (uuid/v1)}]
    (volatile! step)))

(defn simple-step-continue
  "Wrapper to remove state from call"
  [continue]
  (fn simple-step-continue
    [step _state]
    (continue step)
    true))

(defn ->SimpleStep
  "Create a new step volatile map with validation."
  [continue]
  (->> continue
       (simple-step-continue)
       (make-base-step :step/simple)
       (validate-step)))

(defmulti continue!
  "Side-effecting. Must be idempotent."
  (fn [step _state] (:type @step)))

(defmethod continue! :default
  [step state]
  (let [continue (:continue @step)]
    (continue step state)))

(defmulti complete?
  "Is the step complete?"
  (fn [step] (:type @step)))

(defmethod complete? :default
  [step] (:complete? @step))

(defmulti complete!
  "Simple helper function to complete the step."
  (fn [step] (:type @step)))

(defmethod complete! :default
  [step] (vswap! step assoc :complete? true))

(defmulti active?
  "Is the step active? aka Should we perform the continue again or no"
  (fn [step] (:type @step)))

(defmethod active? :default
  [step] (:active? @step))

(defmulti activate!
  "Make the step :active? true"
  (fn [step] (:type @step)))

(defmethod activate! :default
  [step] (vswap! step assoc :active? true))

(defmulti on-card-clicked!
  "Called when a card is a clicked"
  (fn [step] (:type @step)))

(defmethod on-card-clicked! :default
  [step]
  (let [on-card-clicked (:on-card-clicked @step)]
    (on-card-clicked step)))

(defmulti on-prompt-clicked!
  "Called when a button in a menu is clicked"
  (fn [step] (:type @step)))

(defmethod on-prompt-clicked! :default
  [step]
  (let [on-prompt-clicked (:on-prompt-clicked @step)]
    (on-prompt-clicked step)))
