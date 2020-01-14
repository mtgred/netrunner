(ns game.macros-test
  (:require [game.core :as core]
            [game.core.card :refer [get-card]]
            [game.utils :refer [side-str]]
            [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [game.utils-test :refer :all]
            [jinteki.utils :as jutils]))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'get-run (fn [] (:run @~'state))
         ~'hand-size (fn [side#] (core/hand-size ~'state side#))
         ~'refresh (fn [card#]
                     ;; ;; uncommenting the below two assertions causes a looot of tests to fail
                     ;; (is ~'card "card passed to refresh should not be nil")
                     (let [~'ret (get-card ~'state card#)]
                       ;; (is ~'ret "(refresh card) is nil - if this is intended, use (core/get-card state card)")
                       ~'ret))
         ~'prompt-map (fn [side#] (-> @~'state side# :prompt first))
         ~'prompt-type (fn [side#] (:prompt-type (~'prompt-map side#)))
         ~'prompt-buttons (fn [side#] (->> (~'prompt-map side#) :choices (map :value)))
         ~'prompt-titles (fn [side#] (map :title (~'prompt-buttons side#)))
         ~'prompt-fmt (fn [side#]
                        (let [prompt# (~'prompt-map side#)
                              choices# (:choices prompt#)
                              choices# (cond
                                         (nil? choices#) nil
                                         (sequential? choices#) choices#
                                         :else [choices#])
                              prompt-type# (:prompt-type prompt#)]
                          (str (side-str side#) ": " (:msg prompt# "") "\n"
                               "Type: " (if (some? prompt-type#) prompt-type# "nil") "\n"
                               (join "\n" (map #(str "[ " (or (get-in % [:value :title])
                                                              (:value %)
                                                              %
                                                              "nil") " ]") choices#)) "\n")))]
     ~@body))

(defmacro deftest-pending [name & body]
  (let [message (str "\n" name " is pending")]
    `(clojure.test/deftest- ~name (println ~message))))

(defmacro changes-val-macro [change-amt val-form msg & body-form]
  `(let [start-val# ~val-form]
     (do ~@body-form)
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
    `(changes-val-macro ~change-amt ~val-form ~msg ~body-form)))

;; Enables you to do this:
;; (is (changes-credits (get-runner) -5
;;   (play-from-hand state :runner "Magnum Opus")))
(defmethod clojure.test/assert-expr 'changes-credits [msg form]
  (let [side (nth form 1)
        change-amt (nth form 2)
        body-form (nth form 3)]
    `(changes-val-macro ~change-amt (:credit ~side) ~msg ~body-form)))
