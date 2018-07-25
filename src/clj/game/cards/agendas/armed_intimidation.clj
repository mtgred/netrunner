(in-ns 'game.cards.agendas)

(def card-definition-armed-intimidation
  {"Armed Intimidation"
   {:async true
    :effect (effect (show-wait-prompt :corp "Runner to suffer 5 meat damage or take 2 tags")
                    (continue-ability :runner
                      {:async true
                       :choices ["Suffer 5 meat damage" "Take 2 tags"]
                       :prompt "Choose Armed Intimidation score effect"
                       :effect (req (clear-wait-prompt state :corp)
                                    (case target
                                      "Suffer 5 meat damage"
                                      (do (damage state :runner eid :meat 5 {:card card :unboostable true})
                                          (system-msg state :runner "chooses to suffer 5 meat damage from Armed Intimidation"))
                                      "Take 2 tags"
                                      (do (gain-tags state :runner eid 2 {:card card})
                                          (system-msg state :runner "chooses to take 2 tags from Armed Intimidation"))))}
                      card nil))}})
