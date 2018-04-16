(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-magnet
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
