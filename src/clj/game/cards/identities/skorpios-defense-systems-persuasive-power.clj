(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-skorpios-defense-systems-persuasive-power
  {"Skorpios Defense Systems: Persuasive Power"
   {:implementation "Manually triggered, no restriction on which cards in Heap can be targeted.  Cannot use on in progress run event"
    :abilities [{:label "Remove a card in the Heap that was just trashed from the game"
                 :delayed-completion true
                 :effect (req (when-not (and (used-this-turn? (:cid card) state)) (active-prompt? state side card)
                                (show-wait-prompt state :runner "Corp to use Skorpios' ability" {:card card})
                                (continue-ability state side {:prompt "Choose a card in the Runner's Heap that was just trashed"
                                                              :once :per-turn
                                                              :choices (req (cancellable
                                                              ; do not allow a run event in progress to get nuked #2963
                                                              (remove #(= (:cid %) (get-in @state [:run :run-effect :card :cid]))
                                                                      (:discard runner))))
                                                              :msg (msg "remove " (:title target) " from the game")
                                                              :effect (req (move state :runner target :rfg)
                                                                           (clear-wait-prompt state :runner)
                                                                           (effect-completed state side eid))
                                                              :cancel-effect (req (clear-wait-prompt state :runner)
                                                                                  (effect-completed state side eid))}
                                                  card nil)))}]}})