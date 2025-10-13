(ns nr.translations
  (:require
   [clojure.string :as str]
   [jinteki.i18n :as i18n]
   [nr.appstate :refer [app-state]]
   [reagent.core :as r]))

(def language-cursor
  (r/cursor app-state [:options :language]))

(defn tr
  ([resource] (tr resource nil))
  ([resource params]
   (i18n/format language-cursor resource params)))

(defn- i18n-keys
  "put params into tr-element as data if needed"
  [params]
  (into {} (map (fn [[k v]] [(keyword (str "data-i18n-param-" (name k))) (str v)])) params))

(defn tr-element
  ([element resource] (tr-element element resource nil))
  ([element resource params]
   [element (merge {:data-i18n-key (first resource)} (i18n-keys params)) (tr resource params)]))

(defn tr-span
  ([resource] (tr-element :span resource nil))
  ([resource params] (tr-element :span resource params)))

(defn clean-input
  [s]
  (assert (seq s) "Given empty string")
  (-> (or s "")
      (str/replace " " "-")
      (str/replace "&" "-")
      (str/replace "'" "-")
      (str/replace "." "")
      (str/lower-case)))

(defn tr-fix-server-name
  [s]
  (let [cleaned (clean-input s)]
    (if-let [[_ num] (re-matches #"server-(\d+)" cleaned)]
      {:msg "server-num" :num num}
      {:msg cleaned})))

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
(defn tr-game-prompt [s] (tr [:game_prompt s] (tr-fix-server-name s)))

(defn tr-data [k data]
  (or (get-in data [:localized k]) (k data)))
