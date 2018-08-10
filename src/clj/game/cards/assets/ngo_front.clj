(in-ns 'game.cards.assets)

(def card-definition-ngo-front
  {"NGO Front"
   (letfn [(builder [cost cred]
             {:advance-counter-cost cost
              :effect (effect (trash card {:cause :ability-cost})
                              (gain-credits cred))
              :label (str "[Trash]: Gain " cred " [Credits]")
              :msg (str "gain " cred " [Credits]")})]
     {:advanceable :always
      :abilities [(builder 1 5)
                  (builder 2 8)]})})
