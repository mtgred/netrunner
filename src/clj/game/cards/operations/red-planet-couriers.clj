(in-ns 'game.core)

(def card-definitions-operations-red-planet-couriers
  {"Red Planet Couriers"
   {:delayed-completion true
    :req (req (some #(can-be-advanced? %) (all-installed state :corp)))
    :prompt "Select an installed card that can be advanced"
    :choices {:req can-be-advanced?}
    :effect (req (let [installed (get-all-installed state)
                       total-adv (reduce + (map :advance-counter
                                                (filter #(:advance-counter %) installed)))]
                   (doseq [c installed]
                     (update! state side (dissoc c :advance-counter)))
                   (set-prop state side target :advance-counter total-adv)
                   (update-all-ice state side)
                   (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                               " advancement tokens to " (card-str state target)))
                   (effect-completed state side eid)))}})
