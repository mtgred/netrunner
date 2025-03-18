(ns game.quotes
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]))


(def quotes-corp-filename "data/quotes-corp.edn")
(def quotes-runner-filename "data/quotes-runner.edn")
(def generic-key "Default")

(def identity-quotes (atom {}))

(defn- load-quote-file [filename]
  (let [file (io/file filename)]
    (when (.exists file)
      (edn/read-string (slurp file)))))

(defn load-quotes! []
  (reset! identity-quotes (merge (load-quote-file quotes-corp-filename)
                                 (load-quote-file quotes-runner-filename))))

(defn- choose-and-repeat [options qty]
  (when (not-empty options)
    (repeat qty (first (shuffle options)))))

(defn make-quote [{player-ident :title} {opp-ident :title opp-faction :faction}]
  (let [generic (get-in @identity-quotes [player-ident generic-key])
        opp-faction (get-in @identity-quotes [player-ident opp-faction])
        opp-specific (get-in @identity-quotes [player-ident opp-ident])
        weighted (concat (choose-and-repeat generic 1)
                         (choose-and-repeat opp-faction 3)
                         (choose-and-repeat opp-specific 20))
        non-blank (filter identity weighted)]
    (if (not-empty non-blank)
      (first (shuffle non-blank))
      "NO QUOTE SRY")))
