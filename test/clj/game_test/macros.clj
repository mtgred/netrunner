(ns game-test.macros
  (:require [game.core :as core]
            [clojure.test :refer :all]))

(defn assert-prompt [state side]
  (is (first (get-in @state [side :prompt]))
      (str "Expected an open " (name side) " prompt")))

(defn prompt-is-type? [state side type]
  (= type (-> @state side :prompt first :prompt-type)))

(defn prompt-choice [state side choice]
  (is (or (number? choice) (string? choice))
      (str "prompt-choice should only be called with strings or numbers as argument - got "
           (if (nil? choice) "nil" choice)))
  (assert-prompt state side)
  (core/resolve-prompt state side {:choice (core/get-card state choice)}))

;start here
(defn prompt-choice-partial [state side choice]
  (core/resolve-prompt state side
                       {:choice (core/get-card state (first (filter #(.contains % choice)
                                                                    (->> @state side :prompt first :choices))))}))

(defn prompt-card [state side card]
  (assert-prompt state side)
  (is (prompt-is-type? side nil)
      (str  "prompt-card should only be used with prompts listing cards, not prompts of type "
            (-> @state side :prompt first :prompt-type)))
  (is (map? card) (str "prompt-card should be called with card map as argument - got "
                       (if (nil? card) "nil" card)))
  (core/resolve-prompt state side {:card (core/get-card state card)}))

(defn prompt-select [state side card]
  (assert-prompt state side)
  (is (prompt-is-type? side :select)
      (str "prompt-select should only be used with prompts "
           "requiring the user to click on cards on grip/table, not "
           (let [type (-> @state side :prompt first :prompt-type)]
             (if type type "nil"))))
  (is (map? card) (str "prompt-select should be called with card map as argument - got "
                       (if (nil? card) "nil" card)))
  (core/select state side {:card (core/get-card state card)}))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'get-run (fn [] (:run @~'state))
         ~'get-hand-size (fn [~'side] (+ (get-in @~'state [~'side :hand-size :base])
                                         (get-in @~'state [~'side :hand-size :mod])))
         ~'refresh (fn [~'card]
                     ;; ;; uncommenting the below two assertions causes a looot of tests to fail
                     ;; (is ~'card "card passed to refresh should not be nil")
                     (let [~'ret (core/get-card ~'state ~'card)]
                       ;; (is ~'ret "(refresh card) is nil - if this is intended, use (core/get-card state card)")
                       ~'ret))
         
         ~'prompt-is-card? (fn [~'side ~'card]
                             (~'assert-prompt ~'state ~'side)
                             (and (:cid ~'card) (-> @~'state ~'side :prompt first :card :cid)
                                  (= (:cid ~'card) (-> @~'state ~'side :prompt first :card :cid))))
         

         ~'prompt-map (fn [side#] (first (get-in @~'state [side# :prompt])))
         ~'prompt-titles (fn [side#] (map #(:title %) (:choices (~'prompt-map side#))))
         ~'prompt? (fn [~'side] (-> @~'state ~'side :prompt first))]
     ~@body))

(defmacro deftest-pending [name & body]
  (let [message (str "\n" name " is pending")]
    `(clojure.test/deftest ~name (println ~message))))

(defmacro changes-val-macro [change-amt val-form body-form msg]
  `(let [start-val# ~val-form]
    ~body-form
    (let [end-val# ~val-form
          actual-change# (- end-val# start-val#)]
      (clojure.test/do-report
        {:type (if (= actual-change# ~change-amt) :pass :fail)
         :actual actual-change#
         :expected ~change-amt
         :message (str "Changed from " start-val# " to " end-val# ", Expected end result of " (+ start-val# ~change-amt) " " ~msg " " '~body-form)}))))

(defmethod clojure.test/assert-expr 'changes-val [msg form]
  (let [change-amt (nth form 1)
        val-form (nth form 2)
        body-form (nth form 3)]
    `(changes-val-macro ~change-amt ~val-form ~body-form ~msg)))

;; Enables you to do this:
;; (is (changes-credits (get-runner) -5
;;   (play-from-hand state :runner "Magnum Opus")))
(defmethod clojure.test/assert-expr 'changes-credits [msg form]
  (let [side (nth form 1)
        change-amt (nth form 2)
        body-form (nth form 3)]
    `(changes-val-macro ~change-amt (:credit ~side) ~body-form ~msg)))
