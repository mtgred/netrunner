(in-ns 'game.core)

(declare run-event)

(def card-events-rebirth
  {"Rebirth"
   {:msg "change identities"
    :prompt "Choose an identity to become"
    :choices (req (let [is-swappable (fn [c] (and (= "Identity" (:type c))
                                             (= (-> @state :runner :identity :faction) (:faction c))
                                             (not (.startsWith (:code c) "00")) ; only draft identities have this
                                             (not (= (:title c) (-> @state :runner :identity :title)))))
                        swappable-ids (filter is-swappable (vals @all-cards))]
                    (cancellable swappable-ids :sorted)))

     :effect (req
               ;; Handle Ayla - Part 1
               (when (-> @state :runner :identity :code (= "13012"))
                 (doseq [c (-> @state :runner :identity :hosted)]
                   (move state side c :temp-nvram)))

               (move state side (last (:discard runner)) :rfg)
               (disable-identity state side)

               ;; Manually reduce the runner's link by old link
               (lose state :runner :link (get-in @state [:runner :identity :baselink]))

               ;; Move the selected ID to [:runner :identity] and set the zone
               (swap! state update-in [side :identity]
                  (fn [x] (assoc (server-card (:title target) (get-in @state [:runner :user]))
                            :zone [:identity])))

               ;; enable-identity does not do everything that init-identity does
               (init-identity state side (get-in @state [:runner :identity]))
               (system-msg state side "NOTE: passive abilities (Kate, Gabe, etc) will incorrectly fire
                if their once per turn condition was met this turn before Rebirth was played.
                Please adjust your game state manually for the rest of this turn if necessary")

               ;; Handle Ayla - Part 2
               (when-not (empty? (-> @state :runner :temp-nvram))
                 (doseq [c (-> @state :runner :temp-nvram)]
                   (host state side (get-in @state [:runner :identity]) c {:facedown true}))))}})