(ns game.core.purging
  (:require
    [game.core.board :refer [all-installed]]
    [game.core.card :refer [get-counters has-subtype?]]
    [game.macros :refer [wait-for]]
    [game.core.eid :refer [make-eid]]
    [game.core.engine :refer [trigger-event-simult]]
    [game.core.ice :refer [update-all-ice]]
    [game.core.props :refer [add-counter]]))

(defn purge
  "Purges viruses."
  [state side eid]
  (wait-for (trigger-event-simult state side (make-eid state eid) :pre-purge nil)
            (let [installed-cards (concat (all-installed state :runner)
                                          (all-installed state :corp))
                  hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
              (doseq [card (concat installed-cards hosted-on-ice)]
                (when (or (has-subtype? card "Virus")
                          (contains? (:counter card) :virus))
                  (add-counter state :runner card :virus (- (get-counters card :virus)))))
              (update-all-ice state side))
            (trigger-event-simult state side eid :purge nil)))
  
