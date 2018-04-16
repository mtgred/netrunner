(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-ngo-front
  {"NGO Front"
   (letfn [(builder [cost cred]
             {:advance-counter-cost cost
              :effect (effect (trash card {:cause :ability-cost}) (gain :credit cred))
              :label (str "[Trash]: Gain " cred " [Credits]")
              :msg (str "gain " cred " [Credits]")})]
     {:advanceable :always
      :abilities [(builder 1 5)
                  (builder 2 8)]})})
