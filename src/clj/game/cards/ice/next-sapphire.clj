(in-ns 'game.core)

(def card-definitions-ice-next-sapphire
  {"NEXT Sapphire"
   {:subroutines [{:label "Draw up to X cards"
                   :prompt "Draw how many cards?"
                   :msg (msg "draw " target " cards")
                   :choices {:number (req (next-ice-count corp))
                             :default (req 1)}
                   :delayed-completion true
                   :effect (effect (draw eid target nil))}
                  {:label "Add up to X cards from Archives to HQ"
                   :prompt "Select cards to add to HQ"
                   :show-discard  true
                   :choices {:req #(and (= "Corp" (:side %)) (= [:discard] (:zone %)))
                             :max (req (next-ice-count corp))}
                   :effect (req (doseq [c targets] (move state side c :hand)))
                   :msg (msg "add "
                             (let [seen (filter :seen targets)
                                   m (count (filter #(not (:seen %)) targets))]
                               (str (join ", " (map :title seen))
                                    (when (pos? m)
                                      (str (when-not (empty? seen) " and ")
                                           (quantify m "unseen card")))))
                             " to HQ")}
                  {:label "Shuffle up to X cards from HQ into R&D"
                   :prompt "Select cards to shuffle into R&D"
                   :choices {:req #(and (= "Corp" (:side %)) (= [:hand] (:zone %)))
                             :max (req (next-ice-count corp))}
                   :effect (req (doseq [c targets] (move state side c :deck))
                                (shuffle! state side :deck))
                   :msg (msg "shuffle " (count targets) " cards from HQ into R&D")}]}})
