(in-ns 'game.cards.operations)

(def card-definition-bad-times
  {"Bad Times"
   {:implementation "Any required program trashing is manual"
    :req (req tagged)
    :msg "force the Runner to lose 2[mu] until the end of the turn"
    :effect (req (lose state :runner :memory 2)
                 (when (neg? (available-mu state))
                   ;; Give runner a toast as well
                   (toast-check-mu state)
                   (system-msg state :runner "must trash programs to free up [mu]")))
    :end-turn {:effect (req (gain state :runner :memory 2)
                            (system-msg state :runner "regains 2[mu]"))}}})
