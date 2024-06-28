(ns nr.gameboard.card-preview
  (:require [cljs.core.async :refer [chan put!]]
            [nr.appstate :refer [app-state]]))

(defonce zoom-channel (chan))

(defn- get-card-data-title [e]
  (let [target (.. e -target)
        title (.getAttribute target "data-card-title")]
    (not-empty title)))

(defn card-preview-mouse-over
  [e channel]
  (.preventDefault e)
  (when-let [title (get-card-data-title e)]
    (when-let [card (get-in @app-state [:all-cards-and-flips title])]
      (put! channel [card e])))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when (get-card-data-title e)
    (put! channel [false false]))
  nil)

(defn card-highlight-mouse-over [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel (select-keys value [:cid])))
  nil)

(defn card-highlight-mouse-out [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel false))
  nil)
