(ns nr.sounds
  (:require
   ["howler" :as howler :refer [Howl]]
   [flatland.ordered.map :refer [ordered-map]]
   [jinteki.utils :refer [str->int]]
   [nr.appstate :refer [app-state]]))

(defn audio-sfx [sound]
  (let [args (clj->js {:src [(str "/sound/" sound ".ogg")
                             (str "/sound/" sound ".mp3")]})]
    [sound (new Howl args)]))

(defonce bespoke-sounds
  (ordered-map
   "archer" {:grouping :archer :default "rez-ice"}
   "bloop" {:grouping :harmonics :default "rez-ice"}
   "echo" {:grouping :harmonics :default "rez-ice"}
   "illumination" {:grouping :illumination :default "play-instant"}
   ;; admittedly, this looks silly, but it's actually really cool, trust me - nbk
   "bling-1" {:grouping :bling :default nil}
   "bling-2" {:grouping :bling :default nil}
   "bling-3" {:grouping :bling :default nil}
   "bling-4" {:grouping :bling :default nil}
   "bling-5" {:grouping :bling :default nil}
   "bling-6" {:grouping :bling :default nil}
   "bling-7" {:grouping :bling :default nil}
   "bling-8" {:grouping :bling :default nil}
   "bling-9" {:grouping :bling :default nil}
   "bling-10" {:grouping :bling :default nil}
   "end-of-the-line" {:grouping :end-of-the-line :default "play-instant"}
   "pulse" {:grouping :harmonics :default "rez-ice"}
   "wave" {:grouping :harmonics :default "rez-ice"}))

(defn select-random-from-grouping
  [key]
  (let [all-keys (vec (keys bespoke-sounds))
        key->group (map (fn [x] [x (get-in bespoke-sounds [x :grouping])]) all-keys)
        relevant (filter #(= (name (second %)) (name key)) key->group)
        just-sounds (vec (shuffle (map first relevant)))]
    (first just-sounds)))

(defn pick-sound
  [name force]
  (if force
    name
    (if-let [sound (get-in bespoke-sounds [name])]
      (if (get-in @app-state [:options :bespoke-sounds (:grouping sound)])
        name
        (:default sound))
      name)))

(defonce sound-names
  ["agenda-score"
   "agenda-steal"
   "click-advance"
   "click-card"
   "click-card-2"
   "click-card-3"
   "click-credit"
   "click-credit-2"
   "click-credit-3"
   "click-run"
   "click-remove-tag"
   "game-end"
   "install-corp"
   "install-runner"
   "play-instant"
   "professional-contacts"
   "rez-ice"
   "rez-other"
   "redirect"
   "run-successful"
   "run-unsuccessful"
   "shuffle"
   "time-out"
   "virus-purge"])

(defn random-sound
  []
  (first (shuffle sound-names)))

(defonce soundbank
  (->> (concat sound-names (vec (keys bespoke-sounds)))
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
  ([sfx] (play-sfx sfx nil))
  ([sfx {:keys [volume force] :as args}]
   (when-let [sfx-key (pick-sound (first sfx) force)]
     (let [sound (get soundbank sfx-key)]
       (.volume sound (/ (or volume (str->int (get-in @app-state [:options :sounds-volume]))) 100))
       (.play sound))
     (play-sfx (rest sfx) args))))

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

