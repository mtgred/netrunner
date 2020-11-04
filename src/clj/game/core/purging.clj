(ns game.core.purging
  (:require
    [game.core.board :refer [all-installed]]
    [game.core.card :refer [get-counters has-subtype?]]
    [game.core.engine :refer [trigger-event]]
    [game.core.ice :refer [update-all-ice]]
    [game.core.props :refer [add-counter]]))

(defn purge
  "Purges viruses."
  [state side]
  (trigger-event state side :pre-purge)
  (let [installed-cards (concat (all-installed state :runner)
                                (all-installed state :corp))
        hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
    (doseq [card (concat installed-cards hosted-on-ice)]
      (when (or (has-subtype? card "Virus")
                (contains? (:counter card) :virus))
        (add-counter state :runner card :virus (- (get-counters card :virus)))))
    (update-all-ice state side))
  (trigger-event state side :purge))
