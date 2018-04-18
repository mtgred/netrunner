(in-ns 'game.core)

(def card-definitions-ice-magnet
  {"Magnet"
   {:delayed-completion true
    :effect (req (let [magnet card]
                   (continue-ability
                     state side
                     {:req (req (some #(some (fn [h] (card-is? h :type "Program")) (:hosted %))
                                      (remove-once #(= (:cid %) (:cid magnet)) (all-active-installed state corp))))
                      :prompt "Select a Program to host on Magnet"
                      :choices {:req #(and (card-is? % :type "Program")
                                           (ice? (:host %))
                                           (not= (:cid (:host %)) (:cid magnet)))}
                      :effect (req (let [hosted (host state side card target)]
                                     (unregister-events state side hosted)
                                     (update! state side (dissoc hosted :abilities))))}
                     card nil)))
    :events {:runner-install {:req (req (= (:cid card) (:cid (:host target))))
                              :effect (req (doseq [c (get-in card [:hosted])]
                                             (unregister-events state side c)
                                             (update! state side (dissoc c :abilities)))
                                           (update-ice-strength state side card))}}
    :subroutines [end-the-run]}})
