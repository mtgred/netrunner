(ns test.macros
  (:require [game.core :as core]
            [clojure.test :refer :all]))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
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
                                  (= ~'type (-> @~'state ~'side :prompt first :prompt-type))))]
     ~@body))
