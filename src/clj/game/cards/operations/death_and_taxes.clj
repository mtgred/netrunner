(in-ns 'game.cards.operations)

(def card-definition-death-and-taxes
  {"Death and Taxes"
   (let [gain-cred-effect {:msg "gain 1 [Credits]"
                           :effect (effect (gain-credits :corp 1))}]
     {:implementation "Credit gain mandatory to save on wait-prompts, adjust credits manually if credit not wanted."
      :events {:runner-install gain-cred-effect
               :runner-trash (assoc gain-cred-effect :req (req (installed? target)))}})})
