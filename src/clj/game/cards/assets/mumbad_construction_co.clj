(in-ns 'game.cards.assets)

(def card-definition-mumbad-construction-co
  {"Mumbad Construction Co."
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}}
    :abilities [{:cost [:credit 2]
                 :req (req (and (pos? (get-counters card :advancement))
                                (not-empty (all-active-installed state :corp))))
                 :label "Move an advancement token to a faceup card"
                 :prompt "Select a faceup card"
                 :choices {:req rezzed?}
                 :msg (msg "move an advancement token to " (card-str state target))
                 :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                 (add-prop target :advance-counter 1 {:placed true}))}]}})
