(ns game.core.virus
  (:require
    [game.core.board :refer [all-active-installed get-all-installed all-installed]]
    [game.core.card :refer [get-counters virus-program?]]))

(defn get-virus-counters
  "Calculate the number of virus counters on the given card, taking Hivemind into account."
  [state card]
  (let [hiveminds (when (virus-program? card)
                    (filter #(= (:title %) "Hivemind") (all-active-installed state :runner)))]
    (reduce + (map #(get-counters % :virus) (cons card hiveminds)))))

(defn count-virus-programs
  "Calculate the number of virus programs in play"
  [state]
  (count (filter virus-program? (all-active-installed state :runner))))

(defn number-of-virus-counters
  "Returns number of actual virus counters (excluding virtual counters from Hivemind)"
  [state]
  (reduce + (map #(get-counters % :virus) (get-all-installed state))))

(defn number-of-runner-virus-counters
  "Returns the number of actual virus counters on Runner cards (excluding virtual counters from Hivemind)"
  [state]
  (reduce + (map #(get-counters % :virus) (all-installed state :runner))))
