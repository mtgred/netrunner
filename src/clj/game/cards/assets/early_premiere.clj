(in-ns 'game.cards.assets)

(def card-definition-early-premiere
  {"Early Premiere"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (some #(and (can-be-advanced? %)
                                            (in-server? %))
                                      (all-installed state :corp)))}
    :abilities [{:cost [:credit 1]
                 :label "Place 1 advancement token on a card that can be advanced in a server"
                 :choices {:req #(and (can-be-advanced? %)
                                      (installed? %)
                                      (in-server? %))} ; should be *in* a server
                 :once :per-turn
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}})
