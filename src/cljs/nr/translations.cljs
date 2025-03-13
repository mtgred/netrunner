(ns nr.translations
  (:require
   [clojure.string :as str]
   [i18n.core :as tr]
   [nr.appstate :refer [app-state]]))

(defn tr
  ([resource] (tr resource nil))
  ([resource params]
   (tr/format app-state resource params)))

(defn fix-string
  [s]
  (let [s (-> (or s "")
              (str/replace " " "-")
              (str/replace "&" "-")
              (str/lower-case))]
    (if (#{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (first s))
      (str "a" s)
      s)))

(defn tr-type [s] (tr [:card-type.name] {:type (fix-string s)}))
(defn tr-side [s] (tr [:side.name] {:side (fix-string s)}))
(defn tr-faction [s] (tr [:faction.name] {:faction (fix-string s)}))
(defn tr-format [s] (tr [:format.name] {:format (fix-string s)}))
(defn tr-room-type [s] (tr [:lobby.type] {:type (fix-string s)}))
(defn tr-pronouns [s] (tr [:pronouns] {:pronoun (fix-string s)}))
(defn tr-set [s] (tr [:set.name] {:name (fix-string s)}))
(defn tr-game-prompt [s] (tr [:game.prompt] {:msg (fix-string s)}))

(defn tr-data [k data]
  (or (get-in data [:localized k]) (k data)))
