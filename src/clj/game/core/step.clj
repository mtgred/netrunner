(ns game.core.step
  (:require
    [malli.core :as m]
    ; [malli.util :as mu]
    [clj-uuid :as uuid]))

(def StepSchema
  [:map {:closed true}
   [:uuid uuid?]
   [:continue-fn [:=> [:cat :map :map] :boolean]]
   [:complete? boolean?]
   [:type qualified-keyword?]])

(def validate-simple-step (m/validator StepSchema))
(def explain-simple-step (m/explainer StepSchema))

(defn SimpleStep?
  "Expects the step to be wrapped in a volatile."
  [step]
  (if (validate-simple-step @step)
    step
    (throw (ex-info "Step isn't valid" {:type (explain-simple-step @step)}))))

(defn ->SimpleStep
  "Create a new step volatile map with validation."
  [continue-fn]
  (let [step {:type :step/simple
              :continue-fn continue-fn
              :uuid (uuid/v1)
              :complete? false}]
    (SimpleStep? (volatile! step))))

; (def PhaseStepSchema
;   (mu/merge
;     StepSchema
;     [:map {:closed true}
;      [:phase string?]]))

; (def PromptStepSchema
;   (mu/merge
;     StepSchema
;     [:map {:closed true}
;      [:active-prompt string?]
;      [:waiting-prompt string?]]))

(defmulti continue!
  "Side-effecting. Must be idempotent."
  (fn [step _] (:type @step)))

(defmethod continue! :default
  [step state]
  (let [continue-fn (:continue-fn @step)]
    (continue-fn step state)))

(defmulti completed?
  "Is the step complete?"
  (fn [step] (:type @step)))

(defmethod completed? :default
  [step] (:complete? @step))

(defmulti complete!
  "Simple helper function to complete the step."
  (fn [step] (:type @step)))

(defmethod complete! :default
  [step] (vswap! step assoc :complete? true))

(defmulti prompt?
  "Is the step a prompt?"
  (fn [step] (:type @step)))

(defmethod prompt? :default
  [_] false)
