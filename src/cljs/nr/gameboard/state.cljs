(ns nr.gameboard.state
  (:require [reagent.core :as r]))

(defonce game-state (r/atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

