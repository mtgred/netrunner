(ns game-test.macros
  (:require [game.core :as core]
            [clojure.test :refer :all]))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'get-hand-size (fn [~'side] (+ (:hand-size-base (~'side @~'state))
                                         (:hand-size-modification (~'side @~'state))))
         ~'refresh (fn [~'card] (core/get-card ~'state ~'card))
         ~'prompt-choice (fn [~'side ~'choice]
                           (is (first (get-in @~'state [~'side :prompt])) "There is a prompt")
                           (core/resolve-prompt ~'state ~'side {:choice (~'refresh ~'choice)}))
         ~'prompt-card (fn [~'side ~'card]
                         (is (first (get-in @~'state [~'side :prompt])) "There is a prompt")
                         (core/resolve-prompt ~'state ~'side {:card (~'refresh ~'card)}))
         ~'prompt-select (fn [~'side ~'card]
                           (is (first (get-in @~'state [~'side :prompt])) "There is a prompt")
                           (core/select ~'state ~'side {:card (~'refresh ~'card)}))
         ~'prompt-is-card? (fn [~'side ~'card]
                             (is (first (get-in @~'state [~'side :prompt])) "There is a prompt")
                             (and (:cid ~'card) (-> @~'state ~'side :prompt first :card :cid)
                                  (= (:cid ~'card) (-> @~'state ~'side :prompt first :card :cid))))
         ~'prompt-is-type? (fn [~'side ~'type]
                             (is (first (get-in @~'state [~'side :prompt])) "There is a prompt")
                             (and ~'type (-> @~'state ~'side :prompt first :prompt-type)
                                  (= ~'type (-> @~'state ~'side :prompt first :prompt-type))))

         ~'prompt-map (fn [side#] (first (get-in @~'state [side# :prompt])))
         ~'prompt-titles (fn [side#] (map #(:title %) (:choices (~'prompt-map side#))))]
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
