(ns tasks.card-rename
  "Rename cards used in decks"
  (:require [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn command
  "Replace a card name in all decks with a new card name."
  [from to]
  (println "Renaming" from "->" to)
  (webdb/connect)
  (try
    (let [orig-count (mc/count db "decks" {"cards.card" from})
          _ (println "Found" orig-count "decks containing" from)
          result (mc/update db "decks" {"cards.card" from}
                            {$set {"cards.$.card" to}}
                            {:multi true})]
      (println "Updated" (.getN result) "decks")
      )
    (finally (webdb/disconnect))))
