(in-ns 'game.core)

(def card-definitions-icebreakers-inversificator
  {"Inversificator"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "No restriction on which pieces of ICE are chosen"
                     :abilities [{:label "Swap the Code Gate you just passed with another ICE"
                                  :once :per-turn
                                  :req (req (:run @state))
                                  :prompt "Select the Code Gate you just passed and another piece of ICE to swap positions"
                                  :choices {:req #(and (installed? %) (ice? %)) :max 2}
                                  :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                  :effect (req (when (= (count targets) 2)
                                                 (swap-ice state side (first targets) (second targets))))}
                                 (break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})})
