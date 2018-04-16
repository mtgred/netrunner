(in-ns 'game.core)

(declare run-event)

(def card-events-singularity
  {"Singularity"
   (run-event
    {:choices (req (filter #(can-run-server? state %) remotes))}
    {:req (req (is-remote? target))
     :replace-access {:mandatory true
                      :msg "trash all cards in the server at no cost"
                      :effect (req (doseq [c (:content run-server)]
                                     (trash state side c)))}})})
