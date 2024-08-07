(ns nr.translation-helpers
  (:require
   [clojure.string :as str]
   [jinteki.utils :refer [slugify]]
   [nr.appstate :refer [app-state]]
   [translations.core :as tr]))

(defn tr [resource & params]
  (apply tr/tr-impl app-state resource params))

(defn tr-string [prefix s]
  (let [side (str/lower-case (str/replace (or s "") " " "-"))
        kw (keyword (str prefix "." side))]
    (tr [kw "Unknown"])))

(defn tr-string-s [prefix s]
  (let [s (-> (or s "")
              (str/replace "&nbsp;" ""))
        side (slugify s)
        kw (keyword (str prefix "." side))]
    (tr [kw s])))

(defn tr-type [s] (tr-string "card-type" s))
(defn tr-side [s] (tr-string "side" s))
(defn tr-faction [s] (tr-string "faction" s))
(defn tr-format [s] (tr-string "format" s))
(defn tr-sort [s] (tr-string "card-browser.sort-by" s))
(defn tr-lobby [s] (tr-string "lobby" s))
(defn tr-pronouns [s] (tr-string "pronouns" s))
(defn tr-watch-join [s] (tr-string "lobby" s))
(defn tr-set [s] (tr-string-s "set" s))
(defn tr-game-prompt [s] (tr-string-s "game-prompt" s))

(defn tr-data [k data]
  (or (get-in data [:localized k]) (k data)))
