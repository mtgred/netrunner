(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-it-department
  {"IT Department"
   {:abilities [{:counter-cost [:power 1]
                 :label "Add strength to a rezzed ICE"
                 :choices {:req #(and (ice? %) (:rezzed %))}
                 :req (req (< 0 (get-in card [:counter :power] 0)))
                 :msg (msg "add strength to a rezzed ICE")
                 :effect (req (update! state side (update-in card [:it-targets (keyword (str (:cid target)))]
                                                             (fnil inc 0)))
                              (update-ice-strength state side target))}
                {:cost [:click 1]
                 :msg "add 1 counter"
                 :effect (effect (add-counter card :power 1))}]
    :events (let [it {:req (req (:it-targets card))
                      :effect (req (update! state side (dissoc card :it-targets))
                                   (update-all-ice state side))}]
              {:pre-ice-strength {:req (req (get-in card [:it-targets (keyword (str (:cid target)))]))
                                  :effect (effect (ice-strength-bonus
                                                    (* (get-in card [:it-targets (keyword (str (:cid target)))])
                                                       (inc (get-in card [:counter :power]))) target))}
               :runner-turn-ends it :corp-turn-ends it})}})