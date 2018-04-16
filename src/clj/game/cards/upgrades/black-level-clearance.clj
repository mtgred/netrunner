(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-black-level-clearance
  {"Black Level Clearance"
   {:events {:successful-run
             {:interactive (req true)
              :req (req this-server)
              :delayed-completion true
              :effect (effect (continue-ability
                                {:prompt "Take 1 brain damage or jack out?"
                                 :player :runner
                                 :choices ["Take 1 brain damage" "Jack out"]
                                 :effect (req (if (= target "Take 1 brain damage")
                                                (damage state side eid :brain 1 {:card card})
                                                (do (jack-out state side nil)
                                                    (swap! state update-in [:runner :prompt] rest)
                                                    (close-access-prompt state side)
                                                    (handle-end-run state side)
                                                    (gain state :corp :credit 5)
                                                    (draw state :corp)
                                                    (system-msg state :corp (str "gains 5 [Credits] and draws 1 card. Black Level Clearance is trashed"))
                                                    (trash state side card)
                                                    (effect-completed state side eid))))}
                               card nil))}}}})
