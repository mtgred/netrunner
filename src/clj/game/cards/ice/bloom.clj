(in-ns 'game.cards.ice)

(def card-definition-bloom
  {"Bloom"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :corp (:zone i))))))]
     {:subroutines
              [{:label "Install a piece of ice from HQ protecting another server, ignoring all costs"
                :prompt "Choose ICE to install from HQ in another server"
                :async true
                :choices {:req #(and (ice? %)
                                     (in-hand? %))}
                :effect (req (let [this (zone->name (second (:zone card)))
                                   nice target]
                               (continue-ability state side
                                                 {:prompt (str "Choose a location to install " (:title target))
                                                  :choices (req (remove #(= this %) (corp-install-list state nice)))
                                                  :async true
                                                  :effect (effect (corp-install nice target {:no-install-cost true}))}
                                                 card nil)))}
               {:label "Install a piece of ice from HQ in the next innermost position, protecting this server, ignoring all costs"
                :prompt "Choose ICE to install from HQ in this server"
                :async true
                :choices {:req #(and (ice? %)
                                     (in-hand? %))}
                :effect (req (let [newice (assoc target :zone (:zone card))
                                   bndx (ice-index state card)
                                   ices (get-in @state (cons :corp (:zone card)))
                                   newices (apply conj (subvec ices 0 bndx) newice (subvec ices bndx))]
                               (swap! state assoc-in (cons :corp (:zone card)) newices)
                               (swap! state update-in (cons :corp (:zone target))
                                      (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                               (card-init state side newice {:resolve-effect false
                                                             :init-data true})
                               (trigger-event state side :corp-install newice)))}]})})
