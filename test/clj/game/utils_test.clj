(ns game.utils-test
  (:require [game.core :as core]
            [game.utils :as utils :refer [side-str same-card?]]
            [clojure.test :refer :all]
            [clojure.string :refer [lower-case split]]))

(defmacro error-wrapper [form]
  `(try ~form
        (catch clojure.lang.ExceptionInfo ex#
          (let [msg# (.getMessage ^Throwable ex#)
                form# (:form (ex-data ex#))
                pred# (:pred (ex-data ex#))
                values# (:values (ex-data ex#))
                result# (:result (ex-data ex#))]
            (do-report {:type :fail :message msg#
                        :expected form# :actual (list '~'not (cons pred# values#))})
            result#))))

(defn is'-predicate
  [msg form]
  (let [pred (first form)
         args (rest form)]
     `(let [values# (list ~@args)
            result# (apply ~pred values#)]
        (if result#
          (do-report {:type :pass, :message ~msg,
                      :expected '~form, :actual (cons ~pred values#)})
          (throw (ex-info ~msg {:form '~form
                                :pred '~pred
                                :values values#
                                :result result#}))))))

(defn is'-any
  [msg form]
  `(let [value# ~form]
     (if value#
       (do-report {:type :pass, :message ~msg,
                   :expected '~form, :actual value#})
       (do-report {:type :fail, :message ~msg,
                   :expected '~form, :actual value#}))
     value#))

(defmacro is'
  ([form] `(is' ~form nil))
  ([form msg]
   (if (and (sequential? form)
            (function? (first form)))
     (is'-predicate msg form)
     (is'-any msg form))))

;;; helper functions for prompt interaction
(defn get-prompt
  [state side]
  (-> @state side :prompt seq first))

(defn prompt-is-type?
  [state side prompt-type]
  (let [prompt (get-prompt state side)]
    (= prompt-type (:prompt-type prompt))))

(defn prompt-is-card?
  [state side card]
  (let [prompt (get-prompt state side)]
    (and (:cid card)
         (get-in prompt [:card :cid])
         (= (:cid card) (get-in prompt [:card :cid])))))

(defn expect-type
  [type-name choice]
  (str "Expected a " type-name ", received [ " choice
       " ] of type " (type choice) "."))

(defn click-card-impl
  [state side card]
  (let [prompt (get-prompt state side)]
    (cond
      ;; Card and prompt types are correct
      (and (prompt-is-type? state side :select)
           (or (map? card)
               (string? card)))
      (if (map? card)
        (core/process-action "select" state side {:card card})
        (let [all-cards (core/get-all-cards state)
              matching-cards (filter #(= card (:title %)) all-cards)]
          (if (= (count matching-cards) 1)
            (core/process-action "select" state side {:card (first matching-cards)})
            (is' (= 1 (count matching-cards))
                 (str "Expected to click card [ " card
                      " ] but found " (count matching-cards)
                      " matching cards. Current prompt is: n" prompt)))))
      ;; Prompt isn't a select so click-card shouldn't be used
      (not (prompt-is-type? state side :select))
      (let [prompt (prompt-is-type? state side :select)]
        (is' (= :select (:prompt-type prompt))
             (str "click-card should only be used with prompts "
                  "requiring the user to click on cards on table")))
      ;; Prompt is a select, but card isn't correct type
      (not (or (map? card)
               (string? card)))
      (is' (true? (or (map? card) (string? card))) (expect-type "card string or map" card)))))

(defmacro click-card
  "Resolves a 'select prompt' by clicking a card. Takes a card map or a card name."
  [state side card]
  `(error-wrapper (click-card-impl ~state ~side ~card)))

(defn click-prompt-impl
  [state side choice & args]
  (let [prompt (get-prompt state side)
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (:counter choices)
          (:number choices))
      (try
        (let [parsed-number (Integer/parseInt choice)]
          (when-not (core/process-action "choice" state side {:choice parsed-number})
            (is' (not true) (str "Parsed number " parsed-number " is incorrect somehow"))))
        (catch Exception e
          (is' (number? (Integer/parseInt choice)) (expect-type "number string" choice))))

      (= :trace (:prompt-type prompt))
      (try
        (let [int-choice (Integer/parseInt choice)
              under (<= int-choice (:choices prompt))]
          (when-not (and under
                         (core/process-action "choice" state side {:choice int-choice}))
            (is' (<= int-choice (:choices prompt))
                 (str (side-str side) " expected to pay [ "
                      int-choice " ] to trace but couldn't afford it."))))
        (catch Exception e
          (is' (number? (Integer/parseInt choice))
               (expect-type "number string" choice))))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (when-not (core/process-action "choice" state side {:choice choice})
        (is' (true? (or (map? choice) (string? choice))) (expect-type "card string or map" choice)))

      ;; Default text prompt
      :else
      (let [choice-fn #(or (= choice (:value %))
                           (= choice (get-in % [:value :title]))
                           (same-card? choice (:value %)))
            idx (or (:idx (first args)) 0)
            chosen (nth (filter choice-fn choices) idx nil)]
        (when-not (and chosen (core/process-action "choice" state side {:choice {:uuid (:uuid chosen)}}))
          (is' (= choice (mapv :value choices))
               (str (side-str side) " expected to click [ "
                    (if (string? choice) choice (:title choice ""))
                    " ] but couldn't find it. Current prompt is: " prompt)))))))

(defmacro click-prompt
  "Clicks a button in a prompt. {choice} is a string or map only, no numbers."
  [state side choice & args]
  `(error-wrapper (click-prompt-impl ~state ~side ~choice ~@args)))

(defn last-log-contains?
  [state content]
  (some? (re-find (re-pattern content)
                  (-> @state :log last :text))))

(defn second-last-log-contains?
  [state content]
  (some? (re-find (re-pattern content)
                  (-> @state :log butlast last :text))))

(defn last-n-log-contains?
  [state n content]
  (some? (re-find (re-pattern content)
                  (:text (nth (-> @state :log reverse) n)))))

(defmethod assert-expr 'last-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         content# ~(nth form 2)
         log# (-> @state# :log last :text)
         found# ~form]
     (do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod assert-expr 'second-last-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         content# ~(nth form 2)
         log# (-> @state# :log butlast last :text)
         found# ~form]
     (do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod assert-expr 'last-n-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         n# ~(nth form 2)
         content# ~(nth form 3)
         log# (:text (nth (-> @state# :log reverse) n#))
         found# ~form]
     (do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod assert-expr 'prompt-is-type?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         given-type# ~(nth form 3)
         prompt-type# (-> @state# side# :prompt :prompt-type)
         found# ~form]
     (do-report
       {:type (if found# :pass :fail)
        :actual prompt-type#
        :expected given-type#
        :message ~msg})
     found#))

(defmethod assert-expr 'prompt-is-card?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         card# ~(nth form 3)
         prompt-card# (-> @state# side# :prompt :card)
         found# ~form]
     (do-report
       {:type (if found# :pass :fail)
        :actual prompt-card#
        :expected card#
        :message ~msg})
     found#))
