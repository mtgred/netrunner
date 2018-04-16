(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-mumbad-construction-co
  {"Mumbad Construction Co."
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}}
    :abilities [{:cost [:credit 2]
                 :req (req (and (> (get card :advance-counter 0) 0)
                                (not-empty (all-active-installed state :corp))))
                 :label "Move an advancement token to a faceup card"
                 :prompt "Select a faceup card"
                 :choices {:req #(rezzed? %)}
                 :msg (msg "move an advancement token to " (card-str state target))
                 :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                 (add-prop target :advance-counter 1 {:placed true}))}]}})