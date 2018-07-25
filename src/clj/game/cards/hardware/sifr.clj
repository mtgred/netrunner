(in-ns 'game.cards.hardware)

(def card-definition-sifr
  {"Şifr"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :req (req (rezzed? current-ice))
                 :msg (msg "lower their maximum hand size by 1 and lower the strength of " (:title current-ice) " to 0")
                 :effect (effect (lose :runner :hand-size 1)
                                 (update! (assoc card :sifr-target current-ice :sifr-used true))
                                 (update-ice-strength current-ice))}]
    :events {:runner-turn-begins {:req (req (:sifr-used card))
                                  :effect (effect (gain :runner :hand-size 1)
                                                  (update! (dissoc card :sifr-used)))}
             :pre-ice-strength {:req (req (= (:cid target) (get-in card [:sifr-target :cid])))
                                :effect (req (let [ice-str (:current-strength target)]
                                               (ice-strength-bonus state side (- ice-str) target)))}
             :run-ends {:effect (effect (update! (dissoc card :sifr-target)))}}}})
