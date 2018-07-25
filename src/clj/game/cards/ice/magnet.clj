(in-ns 'game.cards.ice)

(def card-definition-magnet
  {"Magnet"
   (letfn [(disable-hosted [state side c]
             (doseq [hc (:hosted (get-card state c))]
               (unregister-events state side hc)
               (update! state side (dissoc hc :abilities))))]
     {:async true
      :effect (req (let [magnet card]
                     (wait-for (resolve-ability
                                 state side
                                 {:req (req (some #(some (fn [h] (card-is? h :type "Program")) (:hosted %))
                                                  (remove-once #(= (:cid %) (:cid magnet))
                                                               (filter ice? (all-installed state corp)))))
                                  :prompt "Select a Program to host on Magnet"
                                  :choices {:req #(and (card-is? % :type "Program")
                                                       (ice? (:host %))
                                                       (not= (:cid (:host %)) (:cid magnet)))}
                                  :effect (effect (host card target))}
                                 card nil)
                               (disable-hosted state side card))))
      :derez-effect {:req (req (not-empty (:hosted card)))
                     :effect (req (doseq [c (get-in card [:hosted])]
                                    (card-init state side c {:resolve-effect false})))}
      :events {:runner-install {:req (req (= (:cid card) (:cid (:host target))))
                                :effect (req (disable-hosted state side card)
                                          (update-ice-strength state side card))}}
      :subroutines [end-the-run]})})
