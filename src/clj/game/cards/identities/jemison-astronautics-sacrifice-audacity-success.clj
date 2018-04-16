(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-jemison-astronautics-sacrifice-audacity-success
  {"Jemison Astronautics: Sacrifice. Audacity. Success."
   {:events {:corp-forfeit-agenda
             {:delayed-completion true
              :effect (req (show-wait-prompt state :runner "Corp to place advancement tokens")
                           (let [p (inc (get-agenda-points state :corp target))]
                             (continue-ability state side
                               {:prompt "Select a card to place advancement tokens on with Jemison Astronautics: Sacrifice. Audacity. Success."
                                :choices {:req #(and (installed? %) (= (:side %) "Corp"))}
                                :msg (msg "place " p " advancement tokens on " (card-str state target))
                                :cancel-effect (effect (clear-wait-prompt :runner))
                                :effect (effect (add-prop :corp target :advance-counter p {:placed true})
                                                (clear-wait-prompt :runner))}
                              card nil)))}}}})