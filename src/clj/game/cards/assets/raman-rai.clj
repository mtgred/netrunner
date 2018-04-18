(in-ns 'game.core)

(def card-definitions-assets-raman-rai
  {"Raman Rai"
   {:abilities [{:once :per-turn
                 :label "Lose [Click] and swap a card in HQ you just drew for a card in Archives"
                 :req (req (and (pos? (:click corp))
                                (not-empty (turn-events state side :corp-draw))))
                 :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])]
                                (lose state :corp :click 1)
                                (resolve-ability state side
                                  {:prompt "Choose a card in HQ that you just drew to swap for a card of the same type in Archives"
                                   :choices {:req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                   :effect (req (let [hqcard target
                                                      t (:type hqcard)]
                                                  (resolve-ability state side
                                                    {:show-discard true
                                                     :prompt (msg "Choose an " t " in Archives to reveal and swap into HQ for " (:title hqcard))
                                                     :choices {:req #(and (= (:side %) "Corp")
                                                                          (= (:type %) t)
                                                                          (= (:zone %) [:discard]))}
                                                     :msg (msg "lose [Click], reveal " (:title hqcard) " from HQ, and swap it for " (:title target) " from Archives")
                                                     :effect (req (let [swappedcard (assoc hqcard :zone [:discard])
                                                                        archndx (ice-index state target)
                                                                        arch (get-in @state [:corp :discard])
                                                                        newarch (apply conj (subvec arch 0 archndx) swappedcard (subvec arch archndx))]
                                                                     (swap! state assoc-in [:corp :discard] newarch)
                                                                     (swap! state update-in [:corp :hand]
                                                                            (fn [coll] (remove-once #(= (:cid %) (:cid hqcard)) coll)))
                                                                     (move state side target :hand)))}
                                                   card nil)))}
                                 card nil)))}]}})
