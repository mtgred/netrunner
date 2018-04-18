(in-ns 'game.core)

(def card-definitions-identities-aginfusion-new-miracles-for-a-new-world
  {"AgInfusion: New Miracles for a New World"
   {:abilities [{:once :per-turn
                 :req (req (and (:run @state) (not (rezzed? current-ice)) (can-rez? state side current-ice {:ignore-unique true})))
                 :prompt "Choose another server and redirect the run to its outermost position"
                 :choices (req (cancellable (remove #{(-> @state :run :server central->name)} servers)))
                 :msg (msg "trash the approached ICE. The Runner is now running on " target)
                 :effect (req (let [dest (server->zone state target)]
                                (trash state side current-ice)
                                (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                 :server (rest dest)))))}]}})
