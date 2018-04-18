(in-ns 'game.core)

(def card-definitions-hardware-security-chip
  {"Security Chip"
   {:abilities [{:label "[Trash]: Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (:title target) " until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select one non-Cloud icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                 :effect (effect (pump target (:link runner) :all-run)
                                 (trash (get-card state card) {:cause :ability-cost}))}
                {:label "[Trash]: Add [Link] strength to any Cloud icebreakers until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (count targets) " Cloud icebreakers until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select any number of Cloud icebreakers"
                 :choices {:max 50
                           :req #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (installed? %))}
                 :effect (req (doseq [t targets]
                                (pump state side t (:link runner) :all-run)
                                (update-breaker-strength state side t))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}})
