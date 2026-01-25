(ns nr.gameboard.card-preview
  (:require [cljs.core.async :refer [chan put!]]
            [nr.appstate :refer [app-state]]))

(defonce zoom-channel (chan))

(defn- safe-get-attribute [target attribute]
  (when (.-getAttribute target)
    (.getAttribute target attribute)))

(defn- get-card-data-title [e]
  (let [target (.. e -target)
        title (or (safe-get-attribute target "data-card-title")
                  (when (= "BUTTON" (.-tagName target))
                    (some-> target .-firstChild (safe-get-attribute "data-card-title"))))]
    (not-empty title)))

(defn put-game-card-in-channel
  [card channel]
  (if-let [server-card (get-in @app-state [:all-cards-and-flips (:title card)])]
    (put! channel (merge server-card card))
    (put! channel card))
  nil)

(defn card-preview-mouse-over
  [e channel]
  (.preventDefault e)
  (when-let [title (get-card-data-title e)]
    (when-let [card (get-in @app-state [:all-cards-and-flips title])]
      (put! channel card)))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when (get-card-data-title e)
    (put! channel false))
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

