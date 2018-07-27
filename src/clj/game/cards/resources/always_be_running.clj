(in-ns 'game.cards.resources)

(def card-definition-always-be-running
  {"Always Be Running"
   {:implementation "Run requirement not enforced"
    :events {:runner-turn-begins {:effect (req (toast state :runner
                                              "Reminder: Always Be Running requires a run on the first click" "info"))}}
    :abilities [{:cost [:click 2]
                 :once :per-turn
                 :msg "break 1 subroutine"}]}})
