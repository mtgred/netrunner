(in-ns 'game.cards.resources)

(def card-definition-kasi-string
  {"Kasi String"
   {:events {:run-ends {:req (req (and (first-event? state :runner :run-ends is-remote?)
                                       (not (get-in @state [:run :did-steal]))
                                       (get-in @state [:run :did-access])
                                       (is-remote? (:server run))))
                        :effect (effect (add-counter card :power 1))
                        :msg "add a power counter to itself"}
             :counter-added {:req (req (>= (get-counters (get-card state card) :power) 4))
                             :effect (effect (as-agenda :runner card 1))
                             :msg "add it to their score area as an agenda worth 1 agenda point"}}}})
