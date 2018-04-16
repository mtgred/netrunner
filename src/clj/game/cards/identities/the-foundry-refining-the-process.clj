(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-the-foundry-refining-the-process
  {"The Foundry: Refining the Process"
   {:events
    {:rez {:req (req (and (ice? target) ;; Did you rez and ice just now
                          ;; Are there more copies in the deck or play area (ABT interaction)?
                          ;; (some #(= (:title %) (:title target)) (concat (:deck corp) (:play-area corp)))
                          ;; Based on ruling re: searching and failing to find, we no longer enforce the requirement
                          ;; of there being a target ice to bring into HQ.
                          (empty? (let [rezzed-this-turn (map first (turn-events state side :rez))]
                                    (filter ice? rezzed-this-turn))))) ;; Is this the first ice you've rezzed this turn
           :optional
           {:prompt "Add another copy to HQ?"
            :yes-ability {:effect (req (if-let [found-card (some #(when (= (:title %) (:title target)) %) (concat (:deck corp) (:play-area corp)))]
                                         (do (move state side found-card :hand)
                                             (system-msg state side (str "uses The Foundry to add a copy of "
                                                                         (:title found-card) " to HQ, and shuffles their deck"))
                                             (shuffle! state side :deck))
                                         (do (system-msg state side (str "fails to find a target for The Foundry, and shuffles their deck"))
                                             (shuffle! state side :deck))))}}}}}})