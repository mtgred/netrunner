(in-ns 'game.cards.events)

(def card-definition-lean-and-mean
  {"Lean and Mean"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :msg (msg "make a run on " target (when (< (count (filter #(is-type? % "Program") (all-active-installed state :runner))) 4)
                                        ", adding +2 strength to all icebreakers"))
    :effect (req (when (< (count (filter #(is-type? % "Program") (all-active-installed state :runner))) 4)
                   (doseq [c (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))]
                     (pump state side c 2 :all-run)))
                 (game.core/run state side (make-eid state) target nil card))}})
