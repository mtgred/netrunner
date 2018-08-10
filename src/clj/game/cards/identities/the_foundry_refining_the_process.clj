(in-ns 'game.cards.identities)

(def card-definition-the-foundry-refining-the-process
  {"The Foundry: Refining the Process"
   {:events
    {:rez {:req (req (and (ice? target) ;; Did you rez and ice just now
                          (first-event? state :runner :rez #(ice? (first %)))))
           :optional
           {:prompt "Add another copy to HQ?"
            :yes-ability {:effect (req (if-let [found-card (some #(when (= (:title %) (:title target)) %) (concat (:deck corp) (:play-area corp)))]
                                         (do (move state side found-card :hand)
                                             (system-msg state side (str "uses The Foundry to add a copy of "
                                                                         (:title found-card) " to HQ, and shuffles their deck"))
                                             (shuffle! state side :deck))
                                         (do (system-msg state side (str "fails to find a target for The Foundry, and shuffles their deck"))
                                             (shuffle! state side :deck))))}}}}}})
