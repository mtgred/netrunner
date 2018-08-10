(in-ns 'game.cards.ice)

(def card-definition-sand-storm
  {"Sand Storm"
   {:subroutines [{:req (req (:run @state))
                   :label "Move Sand Storm and the run to another server"
                   :prompt "Choose another server and redirect the run to its outermost position"
                   :choices (req (cancellable servers))
                   :msg (msg "move Sand Storm and the run.  The Runner is now running on " target ". Sand Storm is trashed")
                   :effect (req (let [dest (server->zone state target)]
                                  (swap! state update-in [:run]
                                         #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                 :server (rest dest)))
                                  (trash state side card {:unpreventable true})))}]}})
