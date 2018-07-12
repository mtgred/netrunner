(ns game-test.utils
  (:require [game.core :as core]
            [clojure.test :refer :all]
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

(defn prompt-is-type? [state side type]
  (= type (-> @state side :prompt first :prompt-type)))



;;; everything below here is deprecated. sorta.

(defn assert-prompt [state side]
  (is (first (get-in @state [side :prompt]))
      (str "Expected an open " (name side) " prompt")))


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
  (assert-prompt state side)
  (and (:cid card) (-> @state side :prompt first :card :cid)
       (= (:cid card) (-> @state side :prompt first :card :cid))))


;;; stops here

(defn click-card [state side card]
  "Resolves a 'select prompt' by clicking a card. Can take a card map or a card name. Analogous to prompt-select."
  (assert-prompt state side)
  (is (prompt-is-type? state side :select)
      (str "click-card should only be used with prompts "
           "requiring the user to click on cards on grip/table"))

  (is (or (map? card) (string? card)) "click-card expects a card name or card map")
  (if (map? card)
    (core/select state side {:card card})
    (let [all-cards (flatten (concat (core/get-all-installed state)
                                     (concat (map (fn [side]
                                                    (-> (map #(-> @state side %)
                                                             [:hand :discard :deck :rfg :scored])
                                                        concat flatten))
                                          [:corp :runner]))))
          matching-cards (filter #(= card (:title %)) all-cards)]
      (is (= (count matching-cards) 1)
          (str "Expected to find 1 card with name " card
               ", but found " (count matching-cards)
               ". Try passing card maps instead of card name."))
      (core/select state side {:card (first matching-cards)}))))

(defn click-button
  "Clicks a button in a prompt. Analogous to prompt-choice or prompt"
  [state side button-name-or-num]
  (if (#{"0 [Credits]" "1 [Credits]" "2 [Credits]"} button-name-or-num)
    (click-button state side (read-string (first (clojure.string/split button-name-or-num #" "))))
    (letfn [(choice-to-name [c]
              (cond (string? c) (if (#{"0 [Credits]" "1 [Credits]" "2 [Credits]"} c)
                                  (read-string (first (clojure.string/split c #" ")))
                                  c)
                    (map? c) (:title c) ;cards
                    (int? c) c))]
      (let [prompt (first (get-in @state [side :prompt]))
            choices (:choices prompt)
            button-name (if (not (string? button-name-or-num))
                          (str button-name-or-num)
                          button-name-or-num)]
        (cond (or (= choices :credit)
                  (and (map? choices)
                       (or (:number choices)
                           (:counter choices))))
              (core/resolve-prompt state side {:choice (read-string button-name-or-num)})

              (and (map? choices)       ; Targeted Marketing only, hopefully
                   (:card-title choices))
              (core/resolve-prompt state side {:choice button-name-or-num})
              
              true                      ; old prompt-card 
              (let [choice-titles (map choice-to-name choices)
                    correct-choice-name-pairs (filter #(= button-name (second %))
                                                      (map vector choices choice-titles))
                    correct-choice (first (first correct-choice-name-pairs))]
                (is (= 1 (count correct-choice-name-pairs)) (str "Current prompt has choices " choice-titles
                                                                 " , making argument " button-name " ambiguous or impossible"))
                (if (or (string? correct-choice) (int? correct-choice))
                  (core/resolve-prompt state side {:choice correct-choice})
                  (core/resolve-prompt state side {:card correct-choice})))))))
  nil)
