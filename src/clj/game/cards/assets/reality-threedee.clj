(in-ns 'game.core)

(def card-definitions-assets-reality-threedee
  {"Reality Threedee"
   (let [ability {:effect (req (gain state side :credit (if tagged 2 1)))
                  :label "Gain credits (start of turn)"
                  :once :per-turn
                  :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
   {:effect (effect (gain-bad-publicity :corp 1)
                    (system-msg "takes 1 bad publicity"))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})})
