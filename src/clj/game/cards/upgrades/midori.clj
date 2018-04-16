(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-midori
  {"Midori"
   {:abilities
    [{:req (req this-server)
      :label "Swap the ICE being approached with a piece of ICE from HQ"
      :prompt "Select a piece of ICE"
      :choices {:req #(and (ice? %)
                           (in-hand? %))}
      :once :per-run
      :msg (msg "swap " (card-str state current-ice) " with a piece of ICE from HQ")
      :effect (req (let [hqice target
                         c current-ice]
                     (resolve-ability state side
                       {:effect (req (let [newice (assoc hqice :zone (:zone c))
                                           cndx (ice-index state c)
                                           ices (get-in @state (cons :corp (:zone c)))
                                           newices (apply conj (subvec ices 0 cndx) newice (subvec ices cndx))]
                                       (swap! state assoc-in (cons :corp (:zone c)) newices)
                                       (swap! state update-in [:corp :hand]
                                              (fn [coll] (remove-once #(= (:cid %) (:cid hqice)) coll)))
                                       (trigger-event state side :corp-install newice)
                                       (move state side c :hand)))} card nil)))}]}})