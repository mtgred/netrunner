(ns nr.translations
  (:require
   [clojure.string :as str]
   [jinteki.i18n :as i18n]
   [nr.appstate :refer [app-state]]))

(defn tr
  ([resource] (tr resource nil))
  ([resource params]
   (i18n/format app-state resource params)))

(defn clean-input
  [s]
  (assert (seq s) "Given empty string")
  (-> (or s "")
      (str/replace " " "-")
      (str/replace "&" "-")
      (str/lower-case)))

(defn tr-type [s] (tr [:card-type_name s] {:type (clean-input s)}))
(defn tr-side [s] (tr [:side_name s] {:side (clean-input s)}))
(defn tr-faction [s] (tr [:faction_name s] {:faction (clean-input s)}))
(defn tr-format [s] (tr [:format_name s] {:format (clean-input s)}))
(defn tr-room-type [s] (tr [:lobby_type s] {:type (clean-input s)}))
(defn tr-pronouns [s] (tr [:pronouns s] {:pronoun (clean-input s)}))
(defn tr-set [s]
  (let [s (if (#{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (first s))
            (str "a" s)
            s)]
    (tr [:set_name s] {:name (clean-input s)})))
(defn tr-game-prompt [s] (tr [:game_prompt s] {:msg (clean-input s)}))

(defn tr-data [k data]
  (or (get-in data [:localized k]) (k data)))
