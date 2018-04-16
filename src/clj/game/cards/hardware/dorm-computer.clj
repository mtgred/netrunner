(in-ns 'game.core)

(def card-hardware-dorm-computer
  {"Dorm Computer"
   {:data {:counter {:power 4}}
    :abilities [{:counter-cost [:power 1]
                 :cost [:click 1]
                 :req (req (not run))
                 :prompt "Choose a server"
                 :choices (req runnable-servers)
                 :msg "make a run and avoid all tags for the remainder of the run"
                 :makes-run true
                 :effect (effect (update! (assoc card :dorm-active true))
                                 (run target))}]
    :events {:pre-tag {:req (req (:dorm-active card))
                       :effect (effect (tag-prevent Integer/MAX_VALUE))
                       :msg "avoid all tags during the run"}
             :run-ends {:effect (effect (update! (dissoc card :dorm-active)))}}}})