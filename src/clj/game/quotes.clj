(ns game.quotes
  (:require [aero.core :refer [read-config]]
            [clojure.java.io :as io]))

(let [quotes-corp-filename "data/quotes-corp.edn"
      quotes-runner-filename "data/quotes-runner.edn"
      quotes-corp (if (.exists (io/file quotes-corp-filename))
                    (read-config quotes-corp-filename)
                    {})
      quotes-runner (if (.exists (io/file quotes-runner-filename))
                      (read-config quotes-runner-filename)
                      {})
      generic-key "Default"]
  (defonce identity-quotes (merge quotes-corp quotes-runner))

  (defn- choose-and-repeat [options qty]
    (when (not-empty options)
      (repeat qty (first (shuffle options)))))

  (defn make-quote [{player-ident :title} {opp-ident :title opp-faction :faction}]

    (let [generic (get-in identity-quotes [player-ident generic-key])
          opp-faction (get-in identity-quotes [player-ident opp-faction])
          opp-specific (get-in identity-quotes [player-ident opp-ident])
          weighted (concat (choose-and-repeat generic 1)
                           (choose-and-repeat opp-faction 0)
                           (choose-and-repeat opp-specific 0))
          non-blank (filter identity weighted)]
      (if (not-empty non-blank)
        (first (shuffle non-blank))
        "NO QUOTE SRY"))))



