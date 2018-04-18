(in-ns 'game.core)

(def card-definitions-resources-always-be-running
  {"Always Be Running"
   {:implementation "Run requirement not enforced"
    :events {:runner-turn-begins {:effect (req (toast state :runner
                                              "Reminder: Always Be Running requires a run on the first click" "info"))}}
    :abilities [{:once :per-turn
                 :cost [:click 2]
                 :msg (msg "break 1 subroutine")}]}})
