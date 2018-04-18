(in-ns 'game.core)

(def card-definitions-events-notoriety
  {"Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :effect (effect (as-agenda :runner (first (:play-area runner)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}})
