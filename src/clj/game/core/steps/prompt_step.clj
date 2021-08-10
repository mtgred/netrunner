(ns game.core.steps.prompt-step
  "Prompt Steps are steps that "
  (:require
   [game.core.steps.step :refer [BaseStepSchema complete? make-base-step validate-step]]
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]))

(def PromptStepSchema
  (mu/merge
    BaseStepSchema
    [:map {:closed true}
     [:active-player keyword?]
     [:active-prompt [:fn fn?]]
     [:waiting-prompt [:fn fn?]]]))

(def validate-prompt-step (m/validator PromptStepSchema))
(def explain-prompt-step (m/explainer PromptStepSchema))

(defmethod validate-step :step/prompt
  [step]
  (if (validate-prompt-step @step)
    step
    (let [explained-error (explain-prompt-step @step)]
      (throw (ex-info (str "Prompt step isn't valid: " (pr-str (me/humanize explained-error)))
                      explained-error)))))

(def ButtonSchema
  [:map
   [:text string?]
   [:arg {:optional? true} string?]
   [:command {:optional? true} string?]
   [:uuid {:optional? true} uuid?]])

(def PromptSchema
  [:map
   [:prompt-title {:optional? true} string?]
   [:prompt-text {:optional? true} string?]
   [:buttons [:* ButtonSchema]]
   [:card {:optional? true} map?]
   [:prompt-type {:optional? true} keyword?]
   [:show-discard {:optional? true} boolean?]])

(def validate-prompt (m/validator PromptSchema))

(defn clear-prompt
  [state]
  (doseq [player [:corp :runner]]
    (swap! state assoc-in [player :prompt-state] {})))

(defn add-default-commands-to-buttons
  [prompt]
  (let [button-fn (fn [button]
                    (assoc button
                           :command (:command button :button)
                           :uuid (:uuid prompt)))
        buttons (mapv button-fn (:buttons prompt))]
    (when (seq buttons)
      (assoc prompt :buttons buttons))))

(defn set-prompt
  [step state]
  (let [active-player (:active-player @step)
        active-prompt (:active-prompt @step)
        waiting-prompt (:waiting-prompt @step)]
    (doseq [player [:corp :runner]]
      (if (= player active-player)
        (let [prompt (add-default-commands-to-buttons (active-prompt state))]
          (assert (validate-prompt prompt)
                  (str "Active prompt isn't valid "
                       (->> prompt
                            (m/explain PromptSchema)
                            (me/humanize)
                            (pr-str))))
          (swap! state assoc-in [player :prompt-state] prompt))
        (let [prompt (waiting-prompt state)]
          (assert (validate-prompt prompt)
                  (str "Waiting prompt isn't valid "
                       (->> prompt
                            (m/explain PromptSchema)
                            (me/humanize)
                            (pr-str))))
          (swap! state assoc-in [player :prompt-state] prompt))))))

(defn default-prompt-continue
  [step state]
  (if (complete? step)
      (do (clear-prompt state)
          true)
      (do (set-prompt step state)
          false)))

(defn ->PromptStep
  "A step that displays a prompt to a given player"
  [player active-prompt waiting-prompt]
  (let [step (make-base-step :step/prompt default-prompt-continue)]
    (vswap! step assoc
            :active-player player
            :active-prompt active-prompt
            :waiting-prompt waiting-prompt)
    (validate-step step)))
