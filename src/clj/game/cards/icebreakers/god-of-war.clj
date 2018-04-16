(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-god-of-war
  {"God of War"
   (auto-icebreaker ["All"]
                    {:flags {:runner-phase-12 (req true)}
                     :abilities [(strength-pump 2 1)
                                 {:counter-cost [:virus 1]
                                  :msg "break 1 subroutine"}
                                 {:label "Take 1 tag to place 2 virus counters (start of turn)"
                                  :once :per-turn
                                  :effect (req (when-completed (tag-runner state :runner 1)
                                                               (if (not (get-in @state [:tag :tag-prevent]))
                                                                 (do (add-counter state side card :virus 2)
                                                                     (system-msg state side
                                                                                 (str "takes 1 tag to place 2 virus counters on God of War"))
                                                                     (effect-completed state side eid))
                                                                 (effect-completed state side eid))))}]})})