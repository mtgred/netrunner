(in-ns 'game.cards.agendas)

(def card-definition-advanced-concept-hopper
  {"Advanced Concept Hopper"
   {:events
    {:run
     {:req (req (first-event? state side :run))
      :effect (effect (show-wait-prompt :runner "Corp to use Advanced Concept Hopper")
                      (continue-ability
                        {:player :corp
                         :prompt "Use Advanced Concept Hopper to draw 1 card or gain 1 [Credits]?"
                         :once :per-turn
                         :choices ["Draw 1 card" "Gain 1 [Credits]" "No action"]
                         :effect (req (case target
                                        "Gain 1 [Credits]"
                                        (do (gain-credits state :corp 1)
                                            (system-msg state :corp (str "uses Advanced Concept Hopper to gain 1 [Credits]")))
                                        "Draw 1 card"
                                        (do (draw state :corp)
                                            (system-msg state :corp (str "uses Advanced Concept Hopper to draw 1 card")))
                                        "No action"
                                        (system-msg state :corp (str "doesn't use Advanced Concept Hopper")))
                                      (clear-wait-prompt state :runner)
                                      (effect-completed state side eid))}
                        card nil))}}}})
