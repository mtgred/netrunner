(ns nr.sounds
  (:require
   ["howler" :as howler :refer [Howl]]
   [jinteki.utils :refer [str->int]]
   [nr.appstate :refer [app-state]]))

(defn audio-sfx [sound]
  (let [args (clj->js {:src [(str "/sound/" sound ".ogg")
                             (str "/sound/" sound ".mp3")]})]
    [sound (new Howl args)]))

(defonce soundbank
  (->> ["agenda-score"
        "agenda-steal"
        "click-advance"
        "click-card"
        "click-credit"
        "click-run"
        "click-remove-tag"
        "game-end"
        "install-corp"
        "install-runner"
        "play-instant"
        "rez-ice"
        "rez-other"
        "run-successful"
        "run-unsuccessful"
        "virus-purge"]
       (map audio-sfx)
       (into {})))

(defn play-sound
  [element-id]
  (when (get-in @app-state [:options :lobby-sounds])
    (when-let [element (.getElementById js/document element-id)]
      (.play element))))

(defn resume-sound
  "Chrome doesn't allow audio until audio context is resumed (or created) after a user interaction."
  []
  (when-let [audio-context (aget howler "ctx")]
    (.resume audio-context)))

(defn play-sfx
  "Plays a list of sounds one after another."
  [sfx]
  (when-let [sfx-key (first sfx)]
    (let [sound (get soundbank sfx-key)]
      (.volume sound (/ (str->int (get-in @app-state [:options :sounds-volume])) 100))
      (.play sound))
    (play-sfx (rest sfx))))

(def sfx-last-played (atom nil))

(defn update-audio [{:keys [sfx sfx-current-id]}]
  ;; When it's the first game played with this state or when the sound history comes from different game,
  ;; we skip the cacophony
  (let [sfx-last-played @sfx-last-played]
    (when (and (get-in @app-state [:options :sounds])
               (some? sfx-last-played))
      ;; Skip the SFX from queue with id smaller than the one last played, queue the rest
      (let [sfx-to-play (reduce (fn [sfx-list {:keys [id name]}]
                                  (if (> id sfx-last-played)
                                    (conj sfx-list name)
                                    sfx-list))
                                []
                                sfx)]
        (play-sfx sfx-to-play))))
  ;; Remember the most recent sfx id as last played so we don't repeat it later
  (when sfx-current-id
    (reset! sfx-last-played sfx-current-id)))

