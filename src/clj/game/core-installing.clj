(in-ns 'game.core)

(declare host in-play? make-rid rez run-flag? server-list server->zone set-prop system-msg
         turn-flag? update-breaker-strength update-ice-strength update-run-ice)

;;;; Functions for the installation and deactivation of cards.

;;; Deactivate a card
(defn- dissoc-card
  "Dissoc relevant keys in card"
  [card keep-counter]
  (let [c (dissoc card :current-strength :abilities :rezzed :special :added-virus-counter)
        c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter))]
    (if (and (= (:side c) "Runner") (not= (last (:zone c)) :facedown))
      (dissoc c :installed :facedown :counter :rec-counter :pump :server-target) c)))

(defn- trigger-leave-effect
  "Triggers leave effects for specified card if relevant"
  [state side {:keys [disabled installed rezzed facedown zone] :as card}]
  (when-let [leave-effect (:leave-play (card-def card))]
    (when (and (not disabled)
               (or (and (= (:side card) "Runner") installed (not facedown))
                   rezzed
                   (= (first zone) :current)
                   (= (first zone) :scored)))
      (leave-effect state side (make-eid state) card nil))))

(defn- handle-prevent-effect
  "Handles prevent effects on the card"
  [state card]
  (when-let [prevent (:prevent (card-def card))]
     (doseq [[ptype pvec] prevent]
       (doseq [psub pvec]
         (swap! state update-in [:prevent ptype psub]
                (fn [pv] (remove #(= (:cid %) (:cid card)) pv)))))))

(defn deactivate
  "Deactivates a card, unregistering its events, removing certain attribute keys, and triggering
  some events."
  ([state side card] (deactivate state side card nil))
  ([state side card keep-counter]
   (trigger-leave-effect state side card)
   (handle-prevent-effect state card)
   (unregister-events state side card)
   (when (and (:memoryunits card) (:installed card) (not (:facedown card)))
     (gain state :runner :memory (:memoryunits card)))
   (when (and (find-cid (:cid card) (all-installed state side))
              (or (:rezzed card) (:installed card)))
     (when-let [in-play (:in-play (card-def card))]
       (apply lose state side in-play)))
   (dissoc-card card keep-counter)))


;;; Initialising a card
(defn- ability-init
  "Gets abilities associated with the card"
  [cdef]
  (let [abilities (if (:recurring cdef)
                    (conj (:abilities cdef) {:msg "Take 1 [Recurring Credits]"})
                    (:abilities cdef))]
    (for [ab abilities]
      (assoc (select-keys ab [:cost :pump :breaks]) :label (make-label ab)))))

(defn card-init
  "Initializes the abilities and events of the given card."
  ([state side card] (card-init state side card true))
  ([state side card resolve]
   (let [cdef (card-def card)
         recurring (:recurring cdef)
         abilities (ability-init cdef)
         c (merge card (:data cdef) {:abilities abilities})
         c (if (number? recurring) (assoc c :rec-counter recurring) c)
         c (if (string? (:strength c)) (assoc c :strength 0) c)]
     (when recurring
       (let [r (if (number? recurring)
                 (effect (set-prop card :rec-counter recurring))
                 recurring)]
         (register-events state side
                          {(if (= side :corp) :corp-phase-12 :runner-phase-12)
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
     (when-let [in-play (:in-play cdef)]
       (apply gain state side in-play))
     (get-card state c))))


;;; Intalling a corp card
(defn- corp-can-install?
  "Checks region restrictions"
  [card dest-zone]
  (not (and (has-subtype? card "Region")
            (some #(has-subtype? % "Region") dest-zone))))

(defn- corp-install-asset-agenda
  "Takes care of installing an asset or agenda in a server"
  [state side card dest-zone]
  (when (#{"Asset" "Agenda"} (:type card))
    (when-let [prev-card (some #(when (#{"Asset" "Agenda"} (:type %)) %) dest-zone)]
      (system-msg state side (str "trashes " (card-str state prev-card)))
      (trash state side prev-card {:keep-server-alive true}))))

(defn- corp-install-message
  "Prints the correct install message."
  [state side card server install-state cost-str]
  (let [card-name (if (or (= :rezzed-no-cost install-state)
                          (= :face-up install-state)
                          (:rezzed card))
                    (:title card)
                    (if (ice? card) "ICE" "a card"))
        server-name (if (= server "New remote")
                      (str (remote-num->name (get-in @state [:rid])) " (new remote)")
                      server)]
    (system-msg state side (str (build-spend-msg cost-str "install") card-name
                                (if (ice? card) " protecting " " in ") server-name))))

(defn corp-install-list
  "Returns a list of targets for where a given card can be installed."
  [state card]
  (let [hosts (filter #(when-let [can-host (:can-host (card-def %))]
                        (and (rezzed? %)
                             (can-host state :corp (make-eid state) % [card])))
                      (all-installed state :corp))]
    (concat hosts (server-list state card))))

(defn corp-install
  ([state side card server] (corp-install state side (make-eid state) card server nil))
  ([state side card server args] (corp-install state side (make-eid state) card server args))
  ([state side eid card server {:keys [extra-cost no-install-cost install-state host-card] :as args}]
   (cond
     ;; No server selected; show prompt to select an install site (Interns, Lateral Growth, etc.)
     (not server)
     (continue-ability state side
                       {:prompt (str "Choose a location to install " (:title card))
                        :choices (corp-install-list state card)
                        :delayed-completion true
                        :effect (effect (corp-install eid card target args))}
                       card nil)
     ;; A card was selected as the server; recurse, with the :host-card parameter set.
     (and (map? server) (not host-card))
     (corp-install state side eid card server (assoc args :host-card server))
     ;; A server was selected
     :else
     (let [cdef (card-def card)
           slot (if host-card
                  (:zone host-card)
                  (conj (server->zone state server) (if (ice? card) :ices :content)))
           dest-zone (get-in @state (cons :corp slot))]
       ;; trigger :pre-corp-install before computing install costs so that
       ;; event handlers may adjust the cost.
       (trigger-event state side :pre-corp-install card {:server server :dest-zone dest-zone})
       (let [ice-cost (if (and (ice? card)
                               (not no-install-cost)
                               (not (ignore-install-cost? state side)))
                        (count dest-zone) 0)
             all-cost (concat extra-cost [:credit ice-cost])
             end-cost (install-cost state side card all-cost)
             install-state (or install-state (:install-state cdef))]

         (if (corp-can-install? card dest-zone)
           (if-let [cost-str (pay state side card end-cost)]
             (do (let [c (-> card
                             (assoc :advanceable (:advanceable cdef) :new true)
                             (dissoc :seen :disabled))]
                   (when (= server "New remote")
                     (trigger-event state side :server-created card))
                   (when (not host-card)
                     (corp-install-asset-agenda state side c dest-zone)
                     (corp-install-message state side c server install-state cost-str))

                   (let [moved-card (if host-card
                                      (host state side host-card (assoc c :installed true))
                                      (move state side c slot))]
                     (trigger-event state side :corp-install moved-card)
                     (when (is-type? c "Agenda")
                       (update-advancement-cost state side moved-card))
                     (when (= install-state :rezzed-no-cost)
                       (rez state side moved-card {:ignore-cost :all-costs}))
                     (when (= install-state :rezzed)
                       (rez state side moved-card))
                     (when (= install-state :face-up)
                       (if (:install-state cdef)
                         (card-init state side
                                    (assoc (get-card state moved-card) :rezzed true :seen true) false)
                         (update! state side (assoc (get-card state moved-card) :rezzed true :seen true))))
                     (when-let [dre (:derezzed-events cdef)]
                       (when-not (:rezzed (get-card state moved-card))
                         (register-events state side dre moved-card))))))))
         (clear-install-cost-bonus state side)
         (when-not (:delayed-completion cdef)
           (effect-completed state side eid card)))))))


;;; Installing a runner card
(defn- runner-can-install?
  "Checks if the specified card can be installed.
   Checks uniqueness of card and installed console"
  [state side {:keys [uniqueness] :as card} facedown]
  (and (or (not uniqueness) (not (in-play? state card)) facedown) ; checks uniqueness
       (or (not (and (has-subtype? card "Console") (not facedown)))
           (not (some #(has-subtype? % "Console") (all-installed state :runner)))) ; console check
       (if-let [req (:req (card-def card))]
         (or facedown (req state side (make-eid state) card nil)) ; checks req for install
         true)))

(defn- runner-get-cost
  "Get the total install cost for specified card"
  [state side {:keys [cost memoryunits] :as card}
   {:keys [extra-cost no-cost facedown] :as params}]
  (install-cost state side card
                (concat extra-cost
                        (when (and (not no-cost) (not facedown)) [:credit cost])
                        (when (and memoryunits (not facedown)) [:memory memoryunits]))))

(defn- runner-install-message
  "Prints the correct msg for the card install"
  [state side card-title cost-str
   {:keys [no-cost host-card facedown custom-message] :as params}]
  (if facedown
    (system-msg state side "installs a card facedown")
    (if custom-message
      (system-msg state side custom-message)
      (system-msg state side
                  (str (build-spend-msg cost-str "install") card-title
                       (when host-card (str " on " (:title host-card)))
                       (when no-cost " at no cost"))))))

(defn- handle-virus-counter-flag
  "Deal with setting the added-virus-counter flag"
  [state side installed-card]
  (if (and (has-subtype? installed-card "Virus")
           (pos? (get-in installed-card [:counter :virus] 0)))
    (update! state side (assoc installed-card :added-virus-counter true))))

(defn runner-install
  "Installs specified runner card if able
  Params include extra-cost, no-cost, host-card, facedown and custom-message."
  ([state side card] (runner-install state side (make-eid state) card nil))
  ([state side card params] (runner-install state side (make-eid state) card params))
  ([state side eid card {:keys [host-card facedown] :as params}]
   (if (empty? (get-in @state [side :locked (-> card :zone first)]))
     (if-let [hosting (and (not host-card) (not facedown) (:hosting (card-def card)))]
       (continue-ability state side
                         {:choices hosting
                          :prompt (str "Choose a card to host " (:title card) " on")
                          :effect (effect (runner-install eid card (assoc params :host-card target)))}
                         card nil)
       (do (trigger-event state side :pre-install card)
           (let [cost (runner-get-cost state side card params)]
             (if (runner-can-install? state side card facedown)
               (if-let [cost-str (pay state side card cost)]
                 (let [c (if host-card
                           (host state side host-card card)
                           (move state side card
                                 [:rig (if facedown :facedown (to-keyword (:type card)))]))
                       c (assoc c :installed true :new true)
                       installed-card (if facedown
                                        (update! state side c)
                                        (card-init state side c true))]
                   (runner-install-message state side (:title card) cost-str params)
                   (handle-virus-counter-flag state side installed-card)
                   (when (is-type? card "Resource")
                     (swap! state assoc-in [:runner :register :installed-resource] true))
                   (when (has-subtype? c "Icebreaker")
                     (update-breaker-strength state side c))
                   (trigger-event-simult state side eid :runner-install
                                         nil nil
                                         installed-card))
                 (effect-completed state side eid))
               (effect-completed state side eid)))
           (clear-install-cost-bonus state side)))
     (effect-completed state side eid))))
