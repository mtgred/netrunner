(in-ns 'game.cards.agendas)

(def card-definition-corporate-sales-team
  {"Corporate Sales Team"
   (let [e {:effect (req (when (pos? (get-counters card :credit))
                           (take-credits state :corp 1)
                           (system-msg state :corp (str "uses Corporate Sales Team to gain 1 [Credits]"))
                           (add-counter state side card :credit -1)))}]
     {:effect (effect (add-counter card :credit 10))
      :silent (req true)
      :events {:runner-turn-begins e
               :corp-turn-begins e}})})
