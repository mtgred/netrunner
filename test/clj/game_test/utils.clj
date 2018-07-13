(ns game-test.utils
  (:require [game.core :as core]
            [clojure.test :refer :all]
            [clojure.string :refer [lower-case]]
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

(defn prompt-is-type? [state side prompt-type]
  (= prompt-type (-> @state side :prompt first :prompt-type)))



;;; everything below here is deprecated. sorta.

(defn assert-prompt [state side]
  (let [prompt (-> @state side :prompt)]
    (is (first prompt) (str "Expected an open " (name side) " prompt"))
    (first (seq prompt))))


(defn prompt-choice [state side choice]
  (is (or (number? choice) (string? choice))
      (str "prompt-choice should only be called with strings or numbers as argument - got "
           (if (nil? choice) "nil" choice)))
  (assert-prompt state side)
  (core/resolve-prompt state side {:choice (core/get-card state choice)}))


(defn prompt-choice-partial [state side choice]
  (core/resolve-prompt state side
                       {:choice (core/get-card state (first (filter #(.contains % choice)
                                                                    (->> @state side :prompt first :choices))))}))

(defn prompt-card [state side card]
  (assert-prompt state side)
  (is (prompt-is-type? state side nil)
      (str  "prompt-card should only be used with prompts listing cards, not prompts of type "
            (-> @state side :prompt first :prompt-type)))
  (is (map? card) (str "prompt-card should be called with card map as argument - got "
                       (if (nil? card) "nil" card)))
  (core/resolve-prompt state side {:card (core/get-card state card)}))

(defn prompt-select [state side card]
  (assert-prompt state side)
  (is (prompt-is-type? state side :select)
      (str "prompt-select should only be used with prompts "
           "requiring the user to click on cards on grip/table, not "
           (let [type (-> @state side :prompt first :prompt-type)]
             (if type type "nil"))))
  (is (map? card) (str "prompt-select should be called with card map as argument - got "
                       (if (nil? card) "nil" card)))
  (core/select state side {:card (core/get-card state card)}))

(defn prompt-is-card? [state side card]
  (when-let [prompt (assert-prompt state side)]
    (and (:cid card)
         (-> prompt :card :cid)
         (= (:cid card) (-> prompt :card :cid)))))


(defmacro when-let*
  "Multiple binding version of when-let, from https://stackoverflow.com/a/36160972/3023252"
  [bindings & body]
  (if (seq bindings)
    `(when-let [~(first bindings) ~(second bindings)]
       (when-let* ~(vec (drop 2 bindings)) ~@body))
    `(do ~@body)))

;;; stops here

(defn click-card [state side card]
  "Resolves a 'select prompt' by clicking a card. Can take a card map or a card name."
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
                (str "Expected to click card with name " card
                     ", but found " (count matching-cards)
                     ". Current prompt is: " prompt)))))

      ;; Prompt isn't a select so click-card shouldn't be used
      (not (prompt-is-type? state side :select))
      (is false ;; (prompt-is-type? state side :select)
          (str "click-card should only be used with prompts "
               "requiring the user to click on cards on table"))
      ;; Prompt is a select, but card isn't correct type
      (not (or (map? card)
               (string? card)))
      (is (or (map? card)
              (string? card))
          (str "click-card expects a card name or card map, received " card
               " of type " (type card) ".")))))


(defn click-prompt
  "Clicks a button in a prompt. Analogous to prompt-choice or prompt"
  [state side choice]
  (when-let* [prompt (assert-prompt state side)
              choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (:counter choices)
          (:number choices))
      (if (number? (Integer/parseInt choice))
        (core/resolve-prompt state side {:choice (Integer/parseInt choice)})
        (is (number? (Integer/parseInt choice))
            (str "Expected a number string, received " choice
                 " of type " (type choice) ".")))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (if (or (map? choice)
              (string? choice))
        (core/resolve-prompt state side {:choice choice})
        (is (or (map? choice)
                (string? choice))
            (str "Expected a card string or map, received " choice " of type " (type choice) ".")))

      ;; Default text prompt
      :else
      (let [kw (if (string? (first choices)) :choice :card)
            buttons (filter #(or (= (lower-case choice) (lower-case %))
                                 (= (lower-case choice) (lower-case (:title % ""))))
                            choices)
            button (first buttons)]
        (cond
          (and (= button choice)
               (= 1 (count buttons)))
          (core/resolve-prompt state side {kw button})
          (not= 1 (count buttons))
          (is (= 1 (count buttons)) (str "Expected to click " (if (string? choice) choice (:title choice)) " but found ambiguous choices. "
                                         "Current prompt is: " prompt))
          (not= button choice)
          (is (= button choice) (str "Expected to click " (if (string? choice) choice (:title choice)) " but couldn't find it. "
                                     "Current prompt is: " prompt)))))))
