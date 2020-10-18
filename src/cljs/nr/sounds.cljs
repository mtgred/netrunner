(ns nr.sounds
  (:require [nr.appstate :refer [app-state]]))

(defn play-sound
  [element-id]
  (when (get-in @app-state [:options :lobby-sounds])
    (when-let [element (.getElementById js/document element-id)]
      (.play element))))

(defn resume-sound
  "Chrome doesn't allow audio until audio context is resumed (or created) after a user interaction."
  []
  (when-let [audio-context (aget js/Howler "ctx")]
    (.resume audio-context)))

