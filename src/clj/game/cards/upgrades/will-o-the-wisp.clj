(in-ns 'game.core)

(def card-definitions-upgrades-will-o-the-wisp
  {"Will-o-the-Wisp"
   {:events
    {:successful-run
     {:interactive (req true)
      :delayed-completion true
      :req (req (and this-server
                     (some #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
      :effect (req (show-wait-prompt state :runner "Corp to use Will-o'-the-Wisp")
                   (continue-ability state side
                     {:optional
                      {:prompt "Trash Will-o'-the-Wisp?"
                       :choices {:req #(has-subtype? % "Icebreaker")}
                       :yes-ability {:delayed-completion true
                                     :prompt "Choose an icebreaker used to break at least 1 subroutine during this run"
                                     :choices {:req #(has-subtype? % "Icebreaker")}
                                     :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                                     :effect (effect (trash card)
                                                     (move :runner target :deck)
                                                     (clear-wait-prompt :runner)
                                                     (effect-completed eid card))}
                       :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                    (effect-completed eid card))}}}
                    card nil))}}}})
