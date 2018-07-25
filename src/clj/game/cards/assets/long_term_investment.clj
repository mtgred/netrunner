(in-ns 'game.cards.assets)

(def card-definition-long-term-investment
  {"Long-Term Investment"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :abilities [{:label "Move any number of [Credits] to your credit pool"
                 :req (req (>= (get-counters card :credit) 8))
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (take-credits target))}]
    :events {:corp-turn-begins {:effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credit] to Long-Term Investment")))}}}})
