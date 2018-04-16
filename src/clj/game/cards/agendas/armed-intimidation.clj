(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-armed-intimidation
  {"Armed Intimidation"
   {:delayed-completion true
    :effect (effect (show-wait-prompt :corp "Runner to suffer 5 meat damage or take 2 tags")
                    (continue-ability :runner
                      {:delayed-completion true
                       :choices ["Suffer 5 meat damage" "Take 2 tags"]
                       :prompt "Choose Armed Intimidation score effect"
                       :effect (req (clear-wait-prompt state :corp)
                                    (case target
                                      "Suffer 5 meat damage"
                                      (do (damage state :runner eid :meat 5 {:card card :unboostable true})
                                          (system-msg state :runner "chooses to suffer 5 meat damage from Armed Intimidation"))
                                      "Take 2 tags"
                                      (do (tag-runner state :runner eid 2 {:card card})
                                          (system-msg state :runner "chooses to take 2 tags from Armed Intimidation"))))}
                      card nil))}})
