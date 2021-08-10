(ns game.core.steps.phase-step
  (:require
   [game.core.steps.step :refer [BaseStepSchema make-base-step validate-step]]
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]))

(def PhaseStepSchema
  (mu/merge
    BaseStepSchema
    [:map {:closed true}
     [:phase [:qualified-keyword {:namespace :phase}]]]))

(def validate-phase-step (m/validator PhaseStepSchema))
(def explain-phase-step (m/explainer PhaseStepSchema))

(defmethod validate-step :step/phase
  [step]
  (if (validate-phase-step @step)
    step
    (let [explained-error (explain-phase-step @step)]
      (throw (ex-info (str "Phase step isn't valid: " (pr-str (me/humanize explained-error)))
                      explained-error)))))

(defn ->PhaseStep
  "Phases must queue their own complete! at the end of the steps they queue."
  [phase-name continue]
  (let [step (make-base-step :step/phase continue)]
    (vswap! step assoc :phase phase-name)
    (validate-step step)))
