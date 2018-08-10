(in-ns 'game.cards.resources)

(def card-definition-temujin-contract
  {"Temüjin Contract"
   {:data {:counter {:credit 20}}
    :prompt "Choose a server for Temüjin Contract"
    :choices (req servers)
    :msg (msg "target " target)
    :req (req (not (:server-target card)))
    :effect (effect (update! (assoc card :server-target target)))
    :events {:successful-run
             {:req (req (= (zone->name (get-in @state [:run :server])) (:server-target (get-card state card))))
              :msg "gain 4 [Credits]"
              :effect (req (let [creds (get-counters card :credit)]
                             (gain-credits state side 4)
                             (set-prop state side card :counter {:credit (- creds 4)})
                             (when (zero? (get-counters (get-card state card) :credit))
                               (trash state side card {:unpreventable true}))))}}}})
