(in-ns 'game.core)

(declare host in-play? make-rid rez run-flag? server-list server->zone set-prop system-msg turn-flag?
         update-breaker-strength update-ice-strength update-run-ice)

; Functions for the installation and deactivation of cards.

(defn deactivate
  "Deactivates a card, unregistering its events, removing certain attribute keys, and triggering
  some events."
  ([state side card] (deactivate state side card nil))
  ([state side card keep-counter]
   (let [c (dissoc card :current-strength :abilities :rezzed :special :added-virus-counter)
         c (if (and (= (:side c) "Runner") (not= (last (:zone c)) :facedown))
             (dissoc c :installed :facedown :counter :rec-counter :pump :named-target) c)
         c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter))]
     (when-let [leave-effect (:leave-play (card-def card))]
       (when (or (and (= (:side card) "Runner") (:installed card) (not (:facedown card)))
                 (:rezzed card)
                 (= (first (:zone card)) :current)
                 (not (empty? (filter #(= (:cid card) (:cid %)) (get-in @state [:corp :scored])))))
         (leave-effect state side card nil)))
     (when-let [prevent (:prevent (card-def card))]
       (doseq [[ptype pvec] prevent]
         (doseq [psub pvec]
           (swap! state update-in [:prevent ptype psub] (fn [pv] (remove #(= (:cid %) (:cid card)) pv))))))
     (unregister-events state side card)
     (when (and (:memoryunits card) (:installed card) (not (:facedown card)))
       (gain state :runner :memory (:memoryunits card)))
     c)))

(defn card-init
  "Initializes the abilities and events of the given card."
  ([state side card] (card-init state side card true))
  ([state side card resolve]
   (let [cdef (card-def card)
         abilities (if (:recurring cdef)
                     (conj (:abilities cdef) {:msg "Take 1 [Recurring Credits]"})
                     (:abilities cdef))
         abilities (for [ab abilities]
                     (assoc (select-keys ab [:cost :pump :breaks])
                       :label (or (:label ab) (and (string? (:msg ab)) (capitalize (:msg ab))) "")))
         c (merge card (:data cdef) {:abilities abilities})
         c (if-let [r (:recurring cdef)]
             (if (number? r) (assoc c :rec-counter r) c) c)]
     (when-let [recurring (:recurring cdef)]
       (let [r (if (number? recurring)
                 (effect (set-prop card :rec-counter recurring))
                 recurring)]
         (register-events state side
                          {(if (= side :corp) :corp-turn-begins :runner-turn-begins)
                           {:effect r}} c)))
     (when-let [prevent (:prevent cdef)]
       (doseq [[ptype pvec] prevent]
         (doseq [psub pvec]
           (swap! state update-in [:prevent ptype psub] #(conj % card)))))
     (update! state side c)
     (when-let [events (:events cdef)]
       (register-events state side events c))
     (when resolve
       (resolve-ability state side cdef c nil))
     (get-card state c))))

(defn corp-install
  ([state side card server] (corp-install state side card server nil))
  ([state side card server {:keys [extra-cost no-install-cost install-state] :as args}]
   (if-not server
     (prompt! state side card (str "Choose a server to install " (:title card))
              (server-list state card) {:effect (effect (corp-install card target args))})
     (do (when (= server "New remote")
           (trigger-event state side :server-created card))
         (let [cdef (card-def card)
               c (dissoc (assoc card :advanceable (:advanceable cdef)) :seen)
               slot (conj (server->zone state server) (if (ice? c) :ices :content))
               dest-zone (get-in @state (cons :corp slot))
               install-cost (if (and (ice? c) (not no-install-cost))
                              (count dest-zone) 0)
               install-state (or install-state (:install-state cdef))]
           (when (not (and (has? c :subtype "Region")
                           (some #(has? % :subtype "Region") dest-zone)))
             (when-let [cost-str (pay state side card extra-cost :credit install-cost)]
               (when (#{"Asset" "Agenda"} (:type c))
                 (when-let [prev-card (some #(when (#{"Asset" "Agenda"} (:type %)) %) dest-zone)]
                   (system-msg state side (str "trashes " (card-str state prev-card)))
                   (trash state side prev-card {:keep-server-alive true})))
               (let [card-name (if (or (= :rezzed-no-cost install-state) (= :face-up install-state) (:rezzed c))
                                 (:title card) (if (ice? c) "ICE" "a card"))]
                 (system-msg state side (str (build-spend-msg cost-str "install")
                                             card-name (if (ice? c) " protecting " " in ") server)))
               (let [moved-card (move state side c slot)]
                 (trigger-event state side :corp-install moved-card)
                 (when (= (:type c) "Agenda")
                   (update-advancement-cost state side moved-card))
                 (when (= install-state :rezzed-no-cost)
                   (rez state side moved-card {:no-cost true}))
                 (when (= install-state :rezzed)
                   (rez state side moved-card))
                 (when (= install-state :face-up)
                   (card-init state side (assoc (get-card state moved-card) :rezzed true :seen true) false))))))))))

(defn runner-install
  ([state side card] (runner-install state side card nil))
  ([state side {:keys [title type cost memoryunits uniqueness ] :as card}
    {:keys [extra-cost no-cost host-card facedown custom-message] :as params}]
   (when (not (seq (get-in @state [side :locked (-> card :zone first)])))
     (if-let [hosting (and (not host-card) (not facedown) (:hosting (card-def card)))]
       (resolve-ability state side
                        {:choices hosting
                         :effect (effect (runner-install card (assoc params :host-card target)))} card nil)
       (do
         (trigger-event state side :pre-install card)
         (let [cost (install-cost state side card
                                  (concat extra-cost (when (and (not no-cost) (not facedown)) [:credit cost])
                                          (when (and memoryunits (not facedown)) [:memory memoryunits])))]
           (when (and (or (not uniqueness) (not (in-play? state card)) facedown)
                      (if-let [req (:req (card-def card))]
                        (or facedown (req state side card nil)) true))
             (when-let [cost-str (pay state side card cost)]
               (let [c (if host-card
                         (host state side host-card card)
                         (move state side card [:rig (if facedown :facedown (to-keyword type))]))
                     installed-card (if facedown
                                      (update! state side (assoc c :installed true))
                                      (card-init state side (assoc c :installed true) true))]
                 (if facedown
                   (system-msg state side "installs a card facedown")
                   (if custom-message
                     (system-msg state side custom-message)
                     (system-msg state side (str (build-spend-msg cost-str "install") title
                                                 (when host-card (str " on " (:title host-card)))
                                                 (when no-cost " at no cost")))))
                 ;Apply added-virus-counter flag for this turn if the card enters play with a counter
                 (if (and
                       (contains? installed-card :counter)
                       (contains? installed-card :subtype)
                       (has? card :subtype "Virus")
                       (> (:counter installed-card) 0))
                   (update! state side (assoc installed-card :added-virus-counter true))
                   )
                 (trigger-event state side :runner-install installed-card)
                 (when (has? c :subtype "Icebreaker") (update-breaker-strength state side c))))))
         (when (has? card :type "Resource") (swap! state assoc-in [:runner :register :installed-resource] true))
         (swap! state update-in [:bonus] dissoc :install-cost))))))
