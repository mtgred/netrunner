(in-ns 'game.cards.ice)

(def card-definition-blockchain
  {"Blockchain"
   (letfn [(sub-count [corp] (int (/ (count (filter #(and (is-type? % "Operation") (has-subtype? % "Transaction"))
                                                    (:discard corp)))
                                     2)))]
     {:abilities [{:label "Gain subroutines"
                   :msg (msg (let [c (sub-count corp)]
                               (str "gain " c (pluralize " subroutine" c))))}]
      :subroutines [{:label "Gain 1 [credits], Runner loses 1 [credits]"
                     :msg "gain 1 [credits] and force the Runner to lose 1 [credits]"
                     :effect (effect (gain-credits 1)
                                     (lose-credits :runner 1))}
                    end-the-run]})})
