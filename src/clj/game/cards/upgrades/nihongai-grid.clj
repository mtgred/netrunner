(in-ns 'game.core)

(def card-definitions-upgrades-nihongai-grid
  {"Nihongai Grid"
   {:events
    {:successful-run
     {:interactive (req true)
      :delayed-completion true
      :req (req (and this-server
                     (or (< (:credit runner) 6)
                         (< (count (:hand runner)) 2))
                     (not-empty (:hand corp))))
      :effect (req (show-wait-prompt state :runner "Corp to use Nihongai Grid")
                   (let [top5 (take 5 (:deck corp))]
                     (if (pos? (count top5))
                       (continue-ability state side
                         {:optional
                          {:prompt "Use Nihongai Grid to look at top 5 cards of R&D and swap one with a card from HQ?"
                           :yes-ability
                           {:delayed-completion true
                            :prompt "Choose a card to swap with a card from HQ"
                            :choices top5
                            :effect (req (let [rdc target]
                                           (continue-ability state side
                                             {:delayed-completion true
                                              :prompt (msg "Choose a card in HQ to swap for " (:title rdc))
                                              :choices {:req in-hand?}
                                              :msg "swap a card from the top 5 of R&D with a card in HQ"
                                              :effect (req (let [hqc target
                                                                 newrdc (assoc hqc :zone [:deck])
                                                                 deck (vec (get-in @state [:corp :deck]))
                                                                 rdcndx (first (keep-indexed #(when (= (:cid %2) (:cid rdc)) %1) deck))
                                                                 newdeck (seq (apply conj (subvec deck 0 rdcndx) target (subvec deck rdcndx)))]
                                                             (swap! state assoc-in [:corp :deck] newdeck)
                                                             (swap! state update-in [:corp :hand]
                                                                    (fn [coll] (remove-once #(= (:cid %) (:cid hqc)) coll)))
                                                             (move state side rdc :hand)
                                                             (clear-wait-prompt state :runner)
                                                             (effect-completed state side eid)))}
                                            card nil)))}
                           :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                     (effect-completed state side eid card))}}}
                        card nil)
                       (do (clear-wait-prompt state :runner)
                           (effect-completed state side eid card)))))}}}})
