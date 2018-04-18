(in-ns 'game.core)

(def card-definitions-ice-curtain-wall
  {"Curtain Wall"
   {:subroutines [end-the-run]
    :strength-bonus (req (let [ices (:ices (card->server state card))]
                           (if (= (:cid card) (:cid (last ices))) 4 0)))
    :events (let [cw {:req (req (and (not= (:cid card) (:cid target))
                                     (= (card->server state card) (card->server state target))))
                      :effect (effect (update-ice-strength card))}]
              {:corp-install cw :trash cw :card-moved cw})}})
