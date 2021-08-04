(ns game.core.steps.active-step
  "Active Steps are steps that "
  (:require
   [game.core.steps.step :refer [BaseStepSchema activate! active? complete? continue! make-base-step validate-step]]
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]))

(def ActiveStepSchema
  (mu/merge
    BaseStepSchema
    [:map {:closed true}
     [:active? boolean?]]))

(def validate-active-step (m/validator ActiveStepSchema))
(def explain-active-step (m/explainer ActiveStepSchema))

(defmethod validate-step :step/active
  [step]
  (if (validate-active-step @step)
    step
    (let [explained-error (explain-active-step @step)]
      (throw (ex-info (str "Active step isn't valid: " (pr-str (me/humanize explained-error)))
                      explained-error)))))

(defmethod continue! :step/active
  [step]
  (cond
    (complete? step) true
    (active? step) false
    :else (let [continue-fn (:continue-fn @step)]
            (activate! step)
            (continue-fn step))))

(defn ->ActiveStep
  "Creates a new step meant to wrap `resolve-ability` or `show-prompt` continue-fns
  that should only be executed once."
  [continue-fn]
  (let [step (make-base-step :step/active continue-fn)]
    (vswap! step assoc :active? false)
    (validate-step step)))
