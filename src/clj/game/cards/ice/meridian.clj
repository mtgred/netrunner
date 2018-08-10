(in-ns 'game.cards.ice)

(def card-definition-meridian
  {"Meridian"
   {:subroutines [{:label "Gain 4 [Credits] and end the run, unless the runner adds Meridian to their score area as an agenda worth -1 agenda points"
                   :async true
                   :effect (req (show-wait-prompt state :corp "Runner to choose an option for Meridian")
                                (continue-ability
                                  state :runner
                                  {:prompt "Choose one"
                                   :choices ["End the run" "Add Meridian to score area"]
                                   :player :runner
                                   :async true
                                   :effect (req (if (= target "End the run")
                                                  (do (system-msg state :corp (str "uses Meridian to gain 4 [Credits] and end the run"))
                                                      (clear-wait-prompt state :corp)
                                                      (gain-credits state :corp 4)
                                                      (end-run state :runner eid))
                                                  (do (system-msg state :runner (str "adds Meridian to their score area as an agenda worth -1 agenda points"))
                                                      (clear-wait-prompt state :corp)
                                                      (wait-for (as-agenda state :runner card -1)
                                                                (when current-ice
                                                                  (no-action state side nil)
                                                                  (continue state side nil))
                                                                (effect-completed state side eid)))))}
                                  card nil))}]}})
