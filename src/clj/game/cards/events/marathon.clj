(in-ns 'game.core)

(declare run-event)

(def card-events-marathon
  {"Marathon"
   (run-event
     {:choices (req (filter #(can-run-server? state %) remotes))}
     {:end-run {:effect (req (prevent-run-on-server state card (:server run))
                             (when (:successful run)
                               (system-msg state :runner "gains 1 [Click] and adds Marathon to their grip")
                               (gain state :runner :click 1)
                               (move state :runner (assoc card :zone [:discard]) :hand)))}})})
