(in-ns 'game.cards.ice)

(def card-definition-harvester
  {"Harvester"
   {:subroutines [{:label "Runner draws 3 cards and discards down to maximum hand size"
                   :msg "make the Runner draw 3 cards and discard down to their maximum hand size"
                   :effect (req (draw state :runner 3)
                                (let [delta (- (count (get-in @state [:runner :hand])) (hand-size state :runner))]
                                  (when (pos? delta)
                                    (resolve-ability
                                      state :runner
                                      {:prompt (msg "Select " delta " cards to discard")
                                       :player :runner
                                       :choices {:max delta
                                                 :req #(in-hand? %)}
                                       :effect (req (doseq [c targets]
                                                      (trash state :runner c))
                                                    (system-msg state :runner
                                                                (str "trashes " (join ", " (map :title targets)))))}
                                      card nil))))}]}})
