(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-temujin-contract
  {"Temüjin Contract"
   {:data {:counter {:credit 20}}
    :prompt "Choose a server for Temüjin Contract" :choices (req servers)
    :msg (msg "target " target)
    :req (req (not (:server-target card)))
    :effect (effect (update! (assoc card :server-target target)))
    :events {:successful-run
             {:req (req (= (zone->name (get-in @state [:run :server])) (:server-target (get-card state card))))
              :msg "gain 4 [Credits]"
              :effect (req (let [creds (get-in card [:counter :credit])]
                             (gain state side :credit 4)
                             (set-prop state side card :counter {:credit (- creds 4)})
                             (when (= 0 (get-in (get-card state card) [:counter :credit]))
                               (trash state side card {:unpreventable true}))))}}}})