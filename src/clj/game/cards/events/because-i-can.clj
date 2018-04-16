(in-ns 'game.core)

(declare run-event)

(def card-events-because-i-can
  {"Because I Can"
   (run-event
    {:choices (req (filter #(can-run-server? state %) remotes))}
    {:req (req (is-remote? target))
     :replace-access {:mandatory true
                      :msg "shuffle all cards in the server into R&D"
                      :effect (req (doseq [c (:content run-server)]
                                     (move state :corp c :deck))
                                   (shuffle! state :corp :deck))}})})
