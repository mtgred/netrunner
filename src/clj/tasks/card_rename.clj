(ns tasks.card-rename
  "Rename cards used in decks"
  (:require
    [tasks.setup :refer [connect disconnect]]
    [monger.collection :as mc]
    [monger.operators :refer [$set]]))

(defn command
  "Replace a card name in all decks with a new card name."
  [from to]
  (println "Renaming" from "->" to)
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
    (try
      (let [orig-count (mc/count db "decks" {"cards.card" from})
            _ (println "Found" orig-count "decks containing" from)
            result (mc/update db "decks" {"cards.card" from}
                              {$set {"cards.$.card" to}}
                              {:multi true})]
        (println "Updated" (.getN result) "decks"))
      (finally (disconnect system)))))
