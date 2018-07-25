(in-ns 'game.cards.programs)

(def card-definition-bankroll
  {"Bankroll"
   {:implementation "Bankroll gains credits automatically."
    :events {:successful-run {:effect (effect (add-counter card :credit 1)
                                              (system-msg "places 1 [Credit] on Bankroll"))}}
    :abilities [{:label "[Trash]: Take all credits from Bankroll"
                 :async true
                 :effect (req (let [credits-on-bankroll (get-counters card :credit)]
                                (wait-for (trash state :runner card {:cause :ability-cost})
                                          (take-credits state :runner credits-on-bankroll)
                                          (system-msg state :runner (str "trashes Bankroll and takes "
                                                                         credits-on-bankroll " credits from it")))))}]}})
