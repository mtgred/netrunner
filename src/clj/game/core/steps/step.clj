(ns game.core.steps.step
  (:require
   [clj-uuid :as uuid]
   [malli.core :as m]
   [malli.error :as me]))

(defmulti validate-step
  "Expects the step to be wrapped in a volatile."
  (fn [step] (:type @step)))

(def BaseStepSchema
  [:map {:closed true}
   [:complete? boolean?]
   [:continue-fn [:=> [:cat :map :map] :boolean]]
   [:type [:qualified-keyword {:namespace :step}]]
   [:uuid uuid?]])

(def validate-simple-step (m/validator BaseStepSchema))
(def explain-simple-step (m/explainer BaseStepSchema))

(defmethod validate-step :step/simple
  [step]
  (if (validate-simple-step @step)
    step
    (let [explained-error (explain-simple-step @step)]
      (throw (ex-info (str "Simple step isn't valid: " (pr-str (me/humanize explained-error)))
                      explained-error)))))

(defn make-base-step
  [step-type continue-fn]
  (let [step {:type step-type
              :continue-fn continue-fn
              :uuid (uuid/v1)
              :complete? false}]
    (volatile! step)))

(defn ->SimpleStep
  "Create a new step volatile map with validation."
  [continue-fn]
  (validate-step (make-base-step :step/simple continue-fn)))

(defmulti continue!
  "Side-effecting. Must be idempotent."
  (fn [step] (:type @step)))

(defmethod continue! :default
  [step]
  (let [continue-fn (:continue-fn @step)]
    (continue-fn step)))

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
  "Is the step active? aka Should we perform the continue-fn again or no"
  (fn [step] (:type @step)))

(defmethod active? :default
  [step] (:active? @step))

(defmulti activate!
  "Make the step :active? true"
  (fn [step] (:type @step)))

(defmethod activate! :default
  [step] (vswap! step assoc :active? true))
