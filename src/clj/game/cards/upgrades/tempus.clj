(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-tempus
  {"Tempus"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :effect (req (when (= (first (:zone card)) :deck)
                            (system-msg state :runner (str "accesses Tempus"))))
             :trace {:base 3
                     :msg "make the Runner choose between losing [Click][Click] or suffering 1 brain damage"
                     :delayed-completion true
                     :effect (req (let [tempus card]
                                    (if (< (:click runner) 2)
                                      (do
                                        (system-msg state side "suffers 1 brain damage")
                                        (damage state side eid :brain 1 {:card tempus}))
                                      (do
                                        (show-wait-prompt state :corp "Runner to resolve Tempus")
                                        (continue-ability
                                          state :runner
                                          {:prompt "Lose [Click][Click] or take 1 brain damage?"
                                           :player :runner
                                           :choices ["Lose [Click][Click]" "Take 1 brain damage"]
                                           :delayed-completion true
                                           :effect (req (clear-wait-prompt state :corp)
                                                        (if (.startsWith target "Take")
                                                          (do
                                                            (system-msg state side (str "chooses to take 1 brain damage"))
                                                            (damage state side eid :brain 1 {:card tempus}))
                                                          (do
                                                            (system-msg state side "chooses to lose [Click][Click]")
                                                            (lose state :runner :click 2)
                                                            (effect-completed state side eid))))}
                                          card nil)))))}}}})
