(in-ns 'game.cards.upgrades)

(def card-definition-will-o-the-wisp
  {"Will-o'-the-Wisp"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req (and this-server
                     (some #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
      :effect (req (show-wait-prompt state :runner "Corp to use Will-o'-the-Wisp")
                   (continue-ability state side
                     {:optional
                      {:prompt "Trash Will-o'-the-Wisp?"
                       :choices {:req #(has-subtype? % "Icebreaker")}
                       :yes-ability {:async true
                                     :prompt "Choose an icebreaker used to break at least 1 subroutine during this run"
                                     :choices {:req #(has-subtype? % "Icebreaker")}
                                     :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                                     :effect (effect (trash card)
                                                     (move :runner target :deck)
                                                     (clear-wait-prompt :runner)
                                                     (effect-completed eid))}
                       :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                    (effect-completed eid))}}}
                    card nil))}}}})
