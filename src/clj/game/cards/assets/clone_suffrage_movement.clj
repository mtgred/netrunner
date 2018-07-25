(in-ns 'game.cards.assets)

(def card-definition-clone-suffrage-movement
  {"Clone Suffrage Movement"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (and (some #(is-type? % "Operation") (:discard corp))
                                     unprotected))}
    :abilities [{:label "Add 1 operation from Archives to HQ"
                 :effect (effect (show-wait-prompt :runner "Corp to use Clone Suffrage Movement")
                                 (continue-ability
                                   {:prompt "Select an operation in Archives to add to HQ"
                                    :once :per-turn
                                    :show-discard true
                                    :choices {:req #(and (is-type? % "Operation")
                                                         (= (:zone %) [:discard]))}
                                    :msg (msg "add "
                                              (if (:seen target)
                                                (:title target)
                                                "a facedown card")
                                              " to HQ")
                                    :effect (effect (move target :hand))
                                    :end-effect (effect (clear-wait-prompt :runner))}
                                   card nil))}]}})
