(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-fringe-applications-tomorrow-today
  {"Fringe Applications: Tomorrow, Today"
   {:events
    {:pre-start-game {:effect draft-points-target}
     :runner-turn-begins {:player :corp
                          :req (req (and (not (:disabled card))
                                         (has-most-faction? state :corp "Weyland Consortium")
                                         (some ice? (all-installed state side))))
                          :prompt "Select a piece of ICE to place 1 advancement token on"
                          :choices {:req #(and (installed? %)
                                               (ice? %))}
                          :msg (msg "place 1 advancement token on " (card-str state target))
                          :effect (req (add-prop state :corp target :advance-counter 1 {:placed true}))}}}})