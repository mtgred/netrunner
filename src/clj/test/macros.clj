(ns test.macros
  (:require [game.core :as core]))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'refresh (fn [~'card] (core/get-card ~'state ~'card))] ~@body))