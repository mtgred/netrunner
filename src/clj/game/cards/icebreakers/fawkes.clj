(in-ns 'game.cards.icebreakers)

(def card-definition-fawkes
  {"Fawkes"
   {:implementation "Stealth credit restriction not enforced"
    :abilities [(break-sub 1 1 "Sentry")
                {:label (str "X [Credits]: +X strength for the remainder of the run (using at least 1 stealth [Credits])")
                 :choices :credit
                 :prompt "How many credits?"
                 :effect (effect (pump card target :all-run))
                 :msg (msg "increase strength by " target " for the remainder of the run")}]}})
