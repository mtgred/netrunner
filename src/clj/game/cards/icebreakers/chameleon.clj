(in-ns 'game.cards.icebreakers)

(def card-definition-chameleon
  {"Chameleon"
   {:prompt "Choose one subtype"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "choose " target)
    :effect (effect (update! (assoc card :subtype-target target)))
    :events {:runner-turn-ends {:msg "add itself to Grip" :effect (effect (move card :hand))}}
    :abilities [{:cost [:credit 1] :msg (msg "break 1 " (:subtype-target card) " subroutine")}]}})
