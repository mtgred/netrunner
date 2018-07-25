(in-ns 'game.cards.icebreakers)

(def card-definition-atman
  {"Atman"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :abilities [(break-sub 1 1)]
    :strength-bonus (req (get-counters card :power))
    :events {:counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}})
