(in-ns 'game.cards.operations)

(def card-definition-red-planet-couriers
  {"Red Planet Couriers"
   {:async true
    :req (req (some #(can-be-advanced? %) (all-installed state :corp)))
    :prompt "Select an installed card that can be advanced"
    :choices {:req can-be-advanced?}
    :effect (req (let [installed (get-all-installed state)
                       total-adv (reduce + (map #(get-counters % :advancement) installed))]
                   (doseq [c installed]
                     (set-prop state side c :advance-counter 0))
                   (set-prop state side target :advance-counter total-adv)
                   (update-all-ice state side)
                   (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                               " advancement tokens to " (card-str state target)))
                   (effect-completed state side eid)))}})
