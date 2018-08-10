(in-ns 'game.cards.resources)

(def card-definition-jackpot
  {"Jackpot!"
   (let [jackpot {:interactive (req true)
                  :async true
                  :req (req (= :runner (:as-agenda-side target)))
                  :effect (req (show-wait-prompt state :corp "Runner to use Jackpot!")
                               (continue-ability
                                 state side
                                 {:optional
                                  {:prompt "Trash Jackpot!?"
                                   :no-ability {:effect (effect (clear-wait-prompt :corp))}
                                   :yes-ability
                                   {:prompt "Choose how many [Credit] to take"
                                    :choices {:number (req (get-counters card :credit))}
                                    :async true
                                    :effect (req (gain-credits state :runner target)
                                                 (system-msg state :runner (str "trashes Jackpot! to gain " target " credits"))
                                                 (clear-wait-prompt state :corp)
                                                 (trash state :runner eid card nil))}}}
                                 card nil))}]
     {:events
      {:runner-turn-begins {:effect (effect (add-counter :runner card :credit 1))}
       :agenda-stolen (dissoc jackpot :req)
       :as-agenda jackpot}})})
