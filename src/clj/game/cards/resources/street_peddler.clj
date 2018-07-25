(in-ns 'game.cards.resources)

(def card-definition-street-peddler
  {"Street Peddler"
   {:interactive (req (some #(card-flag? % :runner-install-draw true) (all-active state :runner)))
    :effect (req (doseq [c (take 3 (:deck runner))]
                   (host state side (get-card state card) c {:facedown true})))
    :abilities [{:req (req (not (install-locked? state side)))
                 :prompt "Choose a card on Street Peddler to install"
                 :choices (req (cancellable (filter #(and (not (is-type? % "Event"))
                                                          (runner-can-install? state side % nil)
                                                          (can-pay? state side nil (modified-install-cost state side % [:credit -1])))
                                                    (:hosted card))))
                 :msg (msg "install " (:title target) " lowering its install cost by 1 [Credits]")
                 :effect (req
                           (when (can-pay? state side nil (modified-install-cost state side target [:credit -1]))
                             (install-cost-bonus state side [:credit -1])
                             (trash state side (update-in card [:hosted]
                                                          (fn [coll]
                                                            (remove-once #(= (:cid %) (:cid target)) coll)))
                                    {:cause :ability-cost})
                             (runner-install state side (dissoc target :facedown))))}]}})
