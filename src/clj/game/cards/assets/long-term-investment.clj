(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-long-term-investment
  {"Long-Term Investment"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :abilities [{:label "Move any number of [Credits] to your credit pool"
                 :req (req (>= (get-in card [:counter :credit] 0) 8))
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (gain :credit target))}]
    :events {:corp-turn-begins {:effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credit] to Long-Term Investment")))}}}})