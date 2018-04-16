(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-rachel-beckman
  {"Rachel Beckman"
   {:in-play [:click 1 :click-per-turn 1]
    :events {:runner-gain-tag {:effect (effect (trash card {:unpreventable true}))
                               :msg (msg "trashes Rachel Beckman for being tagged")}}
    :effect (req (when tagged
                   (trash state :runner card {:unpreventable true})))
    :reactivate {:effect (req (when tagged
                                (trash state :runner card {:unpreventable true})))}}})