(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-jinteki-replicating-perfection
  {"Jinteki: Replicating Perfection"
   {:events
    {:runner-phase-12 {:effect (req (apply prevent-run-on-server
                                           state card (map first (get-remotes @state))))}
     :run {:once :per-turn
           :req (req (is-central? (:server run)))
           :effect (req (apply enable-run-on-server
                               state card (map first (get-remotes @state))))}}
    :req (req (empty? (let [successes (turn-events state side :successful-run)]
                        (filter #(is-central? %) successes))))
    :effect (req (apply prevent-run-on-server state card (map first (get-remotes @state))))
    :leave-play (req (apply enable-run-on-server state card (map first (get-remotes @state))))}})