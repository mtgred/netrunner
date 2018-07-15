(ns game.quotes
  (:require [aero.core :refer [read-config]]
            [clojure.java.io :as io]))

(let [quotes-corp-filename "data/quotes.edn"
      quotes-corp (if (.exists (io/file quotes-corp-filename))
                    (read-config quotes-corp-filename)
                    {})
      generic-key "Default"
      ]
  (defonce identity-quotes quotes-corp)

  (defn make-quote [{player-ident :title} {opp-ident :title}]
    (let [generic (get-in identity-quotes [player-ident generic-key])
          opp-faction (get-in identity-quotes [player-ident (:faction opp-ident)])
          opp-specific (get-in identity-quotes [player-ident opp-ident])
          one-random (flatten (filter #(when % (-> % shuffle first)) [generic opp-faction opp-specific]))
          non-blank (filter #(pos? (count %)) one-random)]

      (if (not-empty non-blank)
        (first (shuffle non-blank))
        "NO QUOTE SRY"))))



