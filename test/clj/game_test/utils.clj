(ns game-test.utils
  (:require [game.core :as core]
            [game.utils :as utils :refer [side-str when-let*]]
            [clojure.test :refer :all]
            [clojure.string :refer [lower-case split]]
            [monger.core :as mg]
            [monger.collection :as mc]
            [jinteki.cards :refer [all-cards]]))

(defn load-card [title]
  (let [conn (mg/connect {:host "127.0.0.1" :port 27017})
        db (mg/get-db conn "netrunner")
        card (mc/find-maps db "cards" {:title title})
        ret (first card)]
    (mg/disconnect conn)
    ret))

(defn load-cards []
  (let [conn (mg/connect {:host "127.0.0.1" :port 27017})
        db (mg/get-db conn "netrunner")
        cards (doall (mc/find-maps db "cards"))]
    (mg/disconnect conn)
    cards))

(defn qty [card amt]
  (let [loaded-card (if (string? card) (@all-cards card) card)]
    (when-not loaded-card
      (throw (Exception. (str card " not found in @all-cards"))))
    {:card loaded-card :qty amt}))

(defn make-deck [identity deck]
  {:identity identity
   :deck (map #(if (string? %) (qty % 1) %) deck)})

(defn default-corp
  ([] (default-corp [(qty "Hedge Fund" 3)]))
  ([deck] (make-deck "Custom Biotics: Engineered for Success" deck)))

(defn default-runner
  ([] (default-runner [(qty "Sure Gamble" 3)]))
  ([deck] (make-deck "The Professor: Keeper of Knowledge" deck)))


;;; helper functions for prompt interaction

(defn assert-prompt [state side]
  (let [prompt (-> @state side :prompt)]
    (is (first prompt) (str "Expected an open " (name side) " prompt"))
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
  (when-let [prompt (assert-prompt state side)]
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
                     ". Current prompt is: " prompt)))))

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
  (when-let* [prompt (assert-prompt state side)
              choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (:counter choices)
          (:number choices)
          (= :psi (:prompt-type prompt)))
      (cond
        ;; Psi games
        (= :psi (:prompt-type prompt))
        (let [choice (-> (str choice) (split #" ") first)]
          (core/resolve-prompt state side {:choice choice}))
        ;; All others
        (number? (Integer/parseInt choice))
        (core/resolve-prompt state side {:choice (Integer/parseInt choice)})
        ;; Incorrect input
        :else
        (is (number? (Integer/parseInt choice))
            (expect-type "number string" choice)))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (if (or (map? choice)
              (string? choice))
        (core/resolve-prompt state side {:choice choice})
        (is (or (map? choice)
                (string? choice))
            (expect-type "card string or map" choice)))

      ;; Default text prompt
      :else
      (let [kw (if (string? choice) :choice :card)
            buttons (filter #(or (= (lower-case choice) (lower-case %))
                                 (= (lower-case choice) (lower-case (:title % ""))))
                            choices)
            button (first buttons)]
        (if (= choice button)
          (core/resolve-prompt state side {kw button})
          (is (= choice choices) (str (side-str side) " expected to click [ "
                                      (if (string? choice) choice (:title choice))
                                      " ] but couldn't find it. Current prompt is: " prompt)))))))
