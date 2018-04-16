(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-woman-in-the-red-dress
  {"Woman in the Red Dress"
   (let [ability {:msg (msg "reveal " (:title (first (:deck corp))) " on the top of R&D")
                  :label "Reveal the top card of R&D (start of turn)"
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (show-wait-prompt :runner "Corp to decide whether or not to draw with Woman in the Red Dress")
                                  (resolve-ability
                                    {:optional
                                     {:player :corp
                                      :prompt (msg "Draw " (:title (first (:deck corp))) "?")
                                      :yes-ability {:effect (effect (clear-wait-prompt :runner)
                                                                    (system-msg (str "draws " (:title (first (:deck corp)))))
                                                                    (draw))}
                                      :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                                   (system-msg "doesn't draw with Woman in the Red Dress"))}}}
                                    card nil))}]
   {:events {:runner-turn-begins ability}
    :abilities [ability]})})