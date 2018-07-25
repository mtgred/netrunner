(in-ns 'game.cards.events)

(def card-definition-notoriety
  {"Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :async true
    :effect (req (as-agenda state :runner eid (first (:play-area runner)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}})
