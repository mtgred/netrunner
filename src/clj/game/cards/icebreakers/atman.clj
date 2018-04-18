(in-ns 'game.core)

(def card-definitions-icebreakers-atman
  {"Atman"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :abilities [(break-sub 1 1)]
    :strength-bonus (req (get-in card [:counter :power] 0))
    :events {:counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}})
