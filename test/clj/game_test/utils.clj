(ns game-test.utils
  (:require [game.core :as core]
            [game.utils :as utils :refer [side-str]]
            [clojure.test :refer :all]
            [clojure.string :refer [lower-case split]]))

;;; helper functions for prompt interaction
(defn assert-prompt [state side]
  (let [prompt (-> @state side :prompt)]
    (is (first prompt) (str "Expected an open " (side-str side) " prompt"))
    (first (seq prompt))))

(defn prompt-is-type? [state side prompt-type]
  (let [prompt (-> @state side :prompt first)]
    (= prompt-type (:prompt-type prompt))))

(defn prompt-is-card? [state side card]
  (when-let [prompt (assert-prompt state side)]
    (and (:cid card)
         (-> prompt :card :cid)
         (= (:cid card) (-> prompt :card :cid)))))

(defn expect-type
  [type-name choice]
  (str "Expected a " type-name ", received [ " choice
       " ] of type " (type choice) "."))

(defn click-card
  "Resolves a 'select prompt' by clicking a card. Takes a card map or a card name."
  [state side card]
  (let [prompt (assert-prompt state side)]
    (cond
      ;; Card and prompt types are correct
      (and (prompt-is-type? state side :select)
           (or (map? card)
               (string? card)))
      (if (map? card)
        (core/select state side {:card card})
        (let [all-cards (concat (core/get-all-installed state)
                                (mapcat (fn [side]
                                          (mapcat #(-> @state side %)
                                                  [:hand :discard :deck :rfg :scored :play-area]))
                                        [:corp :runner]))
              matching-cards (filter #(= card (:title %)) all-cards)]
          (if (= (count matching-cards) 1)
            (core/select state side {:card (first matching-cards)})
            (is (= (count matching-cards) 1)
                (str "Expected to click card [ " card
                     " ] but found " (count matching-cards)
                     " matching cards. Current prompt is: \n" prompt)))))
      ;; Prompt isn't a select so click-card shouldn't be used
      (not (prompt-is-type? state side :select))
      (let [prompt (prompt-is-type? state side :select)]
        (is prompt (str "click-card should only be used with prompts "
                        "requiring the user to click on cards on table")))
      ;; Prompt is a select, but card isn't correct type
      (not (or (map? card)
               (string? card)))
      (is (or (map? card)
              (string? card))
          (expect-type "card string or map" card)))))

(defn click-prompt
  "Clicks a button in a prompt. {choice} is a string or map only, no numbers."
  [state side choice]
  (let [prompt (assert-prompt state side)
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (:counter choices)
          (:number choices))
      (when-not (core/resolve-prompt state side {:choice (Integer/parseInt choice)})
        (is (number? (Integer/parseInt choice))
            (expect-type "number string" choice)))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (when-not (core/resolve-prompt state side {:choice choice})
        (is (or (map? choice)
                (string? choice))
            (expect-type "card string or map" choice)))

      ;; Default text prompt
      :else
      (when-not (core/resolve-prompt state side {(if (string? choice) :choice :card) choice})
        (is (= choice (first choices))
            (str (side-str side) " expected to click [ "
                 (if (string? choice) choice (:title choice ""))
                 " ] but couldn't find it. Current prompt is: \n" prompt))))))
