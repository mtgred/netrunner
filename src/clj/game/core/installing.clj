(in-ns 'game.core)

(declare available-mu free-mu host install-locked? make-rid rez run-flag?
         installable-servers server->zone set-prop system-msg turn-flag? in-play?
         update-breaker-strength update-ice-strength update-run-ice use-mu add-sub)

;;;; Functions for the installation and deactivation of cards.

;;; Deactivate a card
(defn- dissoc-card
  "Dissoc relevant keys in card"
  [card keep-counter]
  (let [c (dissoc card :current-strength :abilities :subroutines :runner-abilities :corp-abilities :rezzed :special :new
                  :added-virus-counter :subtype-target :sifr-used :sifr-target :pump :server-target)
        c (assoc c :subroutines (subroutines-init card (card-def card)))
        c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter :extra-advance-counter))]
    c))

(defn- trigger-leave-effect
  "Triggers leave effects for specified card if relevant"
  [state side {:keys [disabled installed rezzed facedown zone host] :as card}]
  (when-let [leave-effect (:leave-play (card-def card))]
    (when (and (not disabled)
               (not (and (runner? card) host (not installed) (not facedown)))
               (or (and (runner? card) installed (not facedown))
                   rezzed
                   (and host (not facedown))
                   (= (first zone) :current)
                   (= (first zone) :scored)))
      (leave-effect state side (make-eid state) card nil))))

(defn deactivate
  "Deactivates a card, unregistering its events, removing certain attribute keys, and triggering
  some events."
  ([state side card] (deactivate state side card nil))
  ([state side {:keys [cid disabled facedown installed memoryunits rezzed] :as card} keep-counter]
   (unregister-events state side card)
   (trigger-leave-effect state side card)
   (when (and memoryunits
              installed
              (not facedown))
     (free-mu state memoryunits))
   (when (and (find-cid cid (all-active-installed state side))
              (not disabled)
              (or rezzed
                  installed))
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

(defn- corp-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (for [ab (:corp-abilities cdef)]
    (assoc (select-keys ab [:cost]) :label (make-label ab))))

(defn- runner-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (for [ab (:runner-abilities cdef)]
    (assoc (select-keys ab [:cost]) :label (make-label ab))))

(defn card-init
  "Initializes the abilities and events of the given card."
  ([state side card] (card-init state side card {:resolve-effect true :init-data true}))
  ([state side card args] (card-init state side (make-eid state) card args))
  ([state side eid card {:keys [resolve-effect init-data] :as args}]
   (let [cdef (card-def card)
         recurring (:recurring cdef)
         abilities (ability-init cdef)
         run-abs (runner-ability-init cdef)
         corp-abs (corp-ability-init cdef)
         c (merge card
                  (when init-data (:data cdef))
                  {:abilities abilities
                   :runner-abilities run-abs
                   :corp-abilities corp-abs})
         c (if (number? recurring) (assoc c :rec-counter recurring) c)
         c (if (string? (:strength c)) (assoc c :strength 0) c)]
     (when recurring
       (let [r (if (number? recurring)
                 (effect (set-prop card :rec-counter recurring))
                 recurring)]
         (register-events state side
                          {(if (= side :corp) :corp-phase-12 :runner-phase-12)
                           {:effect r}} c)))
     (update! state side c)
     (when-let [events (:events cdef)]
       (register-events state side events c))
     (if (and resolve-effect (is-ability? cdef))
       (resolve-ability state side eid cdef c nil)
       (effect-completed state side eid))
     (when-let [in-play (:in-play cdef)]
       (apply gain state side in-play))
     (get-card state c))))


;;; Intalling a corp card
(defn- corp-can-install-reason
  "Checks if the specified card can be installed.
   Returns true if there are no problems
   Returns :region if Region check fails
   Returns :ice if ICE check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card dest-zone]
  (cond
    ;; Region check
    (and (has-subtype? card "Region")
         (some #(has-subtype? % "Region") dest-zone))
    :region
    ;; ICE install prevented by Unscheduled Maintenance
    (and (ice? card)
         (not (turn-flag? state side card :can-install-ice)))
    :ice
    ;; Installing not locked
    (install-locked? state :corp) :lock-install
    ;; no restrictions
    :default true))

(defn- corp-can-install?
  "Checks `corp-can-install-reason` if not true, toasts reason and returns false"
  [state side card dest-zone]
  (let [reason (corp-can-install-reason state side card dest-zone)
        reason-toast #(do (toast state side % "warning") false)
        title (:title card)]
    (case reason
      ;; pass on true value
      true true
      ;; failed region check
      :region
      (reason-toast (str "Cannot install " (:title card) ", limit of one Region per server"))
      ;; failed install lock check
      :lock-install
      (reason-toast (str "Unable to install " title ", installing is currently locked"))
      ;; failed ICE check
      :ice
      (reason-toast (str "Unable to install " title ": can only install 1 piece of ICE per turn")))))

(defn- corp-install-asset-agenda
  "Forces the corp to trash an existing asset or agenda if a second was just installed."
  [state side eid card dest-zone server]
  (let [prev-card (some #(when (#{"Asset" "Agenda"} (:type %)) %) dest-zone)]
    (if (and (#{"Asset" "Agenda"} (:type card))
             prev-card
             (not (:host card)))
      (continue-ability state side {:prompt (str "The " (:title prev-card) " in " server " will now be trashed.")
                                    :choices ["OK"]
                                    :async true
                                    :effect (req (system-msg state :corp (str "trashes " (card-str state prev-card)))
                                                 (if (get-card state prev-card) ; make sure they didn't trash the card themselves
                                                   (trash state :corp eid prev-card {:keep-server-alive true})
                                                   (effect-completed state :corp eid)))}
                       nil nil)
      (effect-completed state side eid))))

(defn- corp-install-message
  "Prints the correct install message."
  [state side card server install-state cost-str args]
  (when (:display-message args true)
    (let [card-name (if (or (= :rezzed-no-cost install-state)
                            (= :face-up install-state)
                            (rezzed? card))
                      (:title card)
                      (if (ice? card) "ICE" "a card"))
          server-name (if (= server "New remote")
                        (str (remote-num->name (get-in @state [:rid])) " (new remote)")
                        server)]
      (system-msg state side (str (build-spend-msg cost-str "install") card-name
                                  (if (ice? card) " protecting " " in ") server-name)))))

(defn corp-install-list
  "Returns a list of targets for where a given card can be installed."
  [state card]
  (let [hosts (filter #(when-let [can-host (:can-host (card-def %))]
                        (and (rezzed? %)
                             (can-host state :corp (make-eid state) % [card])))
                      (all-installed state :corp))]
    (concat hosts (installable-servers state card))))

(defn- corp-install-continue
  "Used by corp-install to actually install the card, rez it if it's supposed to be installed
  rezzed, and calls :corp-install in an awaitable fashion."
  [state side eid card server {:keys [install-state host-card front index] :as args} slot cost-str]
  (let [cdef (card-def card)
        dest-zone (get-in @state (cons :corp slot))
        install-state (or (:install-state cdef) install-state)
        c (-> card
              (assoc :advanceable (:advanceable cdef) :new true)
              (dissoc :seen :disabled))]
    (clear-install-cost-bonus state side)
    (when-not host-card
      (corp-install-message state side c server install-state cost-str args))
    (play-sfx state side "install-corp")

    (let [moved-card (if host-card
                       (host state side host-card (assoc c :installed true))
                       (move state side c slot {:front front
                                                :index index}))]
      (when (agenda? c)
        (update-advancement-cost state side moved-card))

      ;; Check to see if a second agenda/asset was installed.
      (wait-for (corp-install-asset-agenda state side moved-card dest-zone server)
                (letfn [(event [state side eid _]
                          (trigger-event-simult state side eid :corp-install nil (get-card state moved-card)))]
                  (case install-state
                    ;; Ignore all costs. Pass eid to rez.
                    :rezzed-no-cost
                    (wait-for (event state side nil)
                              (rez state side (assoc eid :source moved-card :source-type :rez) moved-card {:ignore-cost :all-costs}))

                    ;; Ignore rez cost only. Pass eid to rez.
                    :rezzed-no-rez-cost
                    (wait-for (event state side nil)
                              (rez state side (assoc eid :source moved-card :source-type :rez) moved-card {:ignore-cost :rez-costs}))

                    ;; Pay costs. Pass eid to rez.
                    :rezzed
                    (wait-for (event state side nil)
                              (rez state side (assoc eid :source moved-card :source-type :rez) moved-card nil))

                    ;; "Face-up" cards. Trigger effect-completed manually.
                    :face-up
                    (if (:install-state cdef)
                      (wait-for (card-init state side
                                           (assoc (get-card state moved-card) :rezzed true :seen true)
                                           {:resolve-effect false
                                            :init-data true})
                                (event state side eid nil))
                      (do (update! state side (assoc (get-card state moved-card) :rezzed true :seen true))
                          (event state side eid nil)))

                    ;; All other cards. Trigger events, which will trigger effect-completed
                    (event state side eid nil))
                  (when-let [dre (:derezzed-events cdef)]
                    (when-not (:rezzed (get-card state moved-card))
                      (register-events state side dre moved-card))))))))

(defn- corp-install-pay
  "Used by corp-install to pay install costs, code continues in corp-install-continue"
  [state side eid card server {:keys [extra-cost ignore-install-cost ignore-all-cost host-card action] :as args} slot]
  (let [dest-zone (get-in @state (cons :corp slot))
        ice-cost (if (and (ice? card)
                          (not ignore-install-cost)
                          (not (ignore-install-cost? state side)))
                   (count dest-zone) 0)
        all-cost (concat extra-cost [:credit ice-cost])
        end-cost (if ignore-all-cost 0 (install-cost state side card all-cost))
        end-fn #(do (clear-install-cost-bonus state side)
                    (effect-completed state side eid))]
    (if (and (corp-can-install? state side card dest-zone)
             (not (install-locked? state :corp)))
      (wait-for (pay-sync state side (make-eid state eid) card end-cost {:action action})
                (if-let [cost-str async-result]
                  (if (= server "New remote")
                    (wait-for (trigger-event-simult state side :server-created nil card)
                              (corp-install-continue state side eid card server args slot cost-str))
                    (corp-install-continue state side eid card server args slot cost-str))
                  (end-fn)))
      (end-fn))))

(defn corp-install
  "Installs a card in the chosen server. If server is nil, asks for server to install in.
  The args input takes the following values:
  :host-card - Card to host on
  :extra-cost - Extra install costs
  :ignore-all-cost - true if install costs should be ignored
  :action - What type of action installed the card
  :install-state - Can be :rezzed-no-cost, :rezzed-no-rez-cost, :rezzed, or :face-up
  :display-message - Print descriptive text to the log window [default=true]"
  ([state side card server] (corp-install state side (make-eid state) card server nil))
  ([state side card server args] (corp-install state side (make-eid state) card server args))
  ([state side eid card server {:keys [host-card] :as args}]
   (cond
     ;; No server selected; show prompt to select an install site (Interns, Lateral Growth, etc.)
     (not server)
     (continue-ability state side
                       {:prompt (str "Choose a location to install " (:title card))
                        :choices (corp-install-list state card)
                        :async true
                        :effect (effect (corp-install eid card target args))}
                       card nil)
     ;; A card was selected as the server; recurse, with the :host-card parameter set.
     (and (map? server) (not host-card))
     (corp-install state side eid card server (assoc args :host-card server))
     ;; A server was selected
     :else
     (let [slot (if host-card
                  (:zone host-card)
                  (conj (server->zone state server) (if (ice? card) :ices :content)))
           dest-zone (get-in @state (cons :corp slot))]
       ;; trigger :pre-corp-install before computing install costs so that
       ;; event handlers may adjust the cost.
       (wait-for (trigger-event-simult state side :pre-corp-install nil card {:server server :dest-zone dest-zone})
                 (corp-install-pay state side eid card server args slot))))))


;;; Installing a runner card
(defn- runner-can-install-reason
  "Checks if the specified card can be installed.
   Checks uniqueness of card and installed console.
   Returns true if there are no problems
   Returns :console if Console check fails
   Returns :unique if uniqueness check fails
   Returns :req if card-def :req check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card facedown]
  (let [card-req (:req (card-def card))
        uniqueness (:uniqueness card)]
    (cond
      ;; Can always install a card facedown
      facedown true
      ;; Console check
      (and (has-subtype? card "Console")
           (some #(has-subtype? % "Console") (all-active-installed state :runner)))
      :console
      ;; Installing not locked
      (install-locked? state :runner) :lock-install
      ;; Uniqueness check
      (and uniqueness (in-play? state card)) :unique
      ;; Req check
      (and card-req (not (card-req state side (make-eid state) card nil))) :req
      ;; Nothing preventing install
      :default true)))

(defn runner-can-install?
  "Checks `runner-can-install-reason` if not true, toasts reason and returns false"
  ([state side card] (runner-can-install? state side card false))
  ([state side card facedown]
   (let [reason (runner-can-install-reason state side card facedown)
         reason-toast #(do (toast state side % "warning") false)
         title (:title card)]
     (case reason
       ;; pass on true value
       true true
       ;; failed unique check
       :unique
       (reason-toast (str "Cannot install a second copy of " title " since it is unique. Please trash currently"
                          " installed copy first"))
       ;; failed install lock check
       :lock-install
       (reason-toast (str "Unable to install " title " since installing is currently locked"))
       ;; failed console check
       :console
       (reason-toast (str "Unable to install " title ": an installed console prevents the installation of a replacement"))
       :req
       (reason-toast (str "Installation requirements are not fulfilled for " title))))))

(defn- runner-get-cost
  "Get the total install cost for specified card"
  [state side {:keys [cost] :as card}
   {:keys [extra-cost ignore-install-cost ignore-all-cost facedown] :as params}]
  (if ignore-all-cost
    [:credit 0]
    (install-cost state side card
                  (concat extra-cost (when (and (not ignore-install-cost)
                                                (not facedown))
                                       [:credit cost])))))

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
                       (when host-card (str " on " (card-str state host-card)))
                       (when no-cost " at no cost"))))))

(defn- handle-virus-counter-flag
  "Deal with setting the added-virus-counter flag"
  [state side installed-card]
  (if (and (has-subtype? installed-card "Virus")
           (pos? (get-counters installed-card :virus)))
    (update! state side (assoc installed-card :added-virus-counter true))))

(defn runner-install
  "Installs specified runner card if able
  Params include extra-cost, no-cost, host-card, facedown and custom-message."
  ([state side card] (runner-install state side (make-eid state {:source nil
                                                                 :source-type :runner-install}) card nil))
  ([state side card params] (runner-install state side (make-eid state {:source nil
                                                                        :source-type :runner-install}) card params))
  ([state side eid card {:keys [host-card facedown no-mu no-msg] :as params}]
   (if (and (empty? (get-in @state [side :locked (-> card :zone first)]))
            (not (install-locked? state :runner)))
     (if-let [hosting (and (not host-card) (not facedown) (:hosting (card-def card)))]
       (continue-ability state side
                         {:choices hosting
                          :prompt (str "Choose a card to host " (:title card) " on")
                          :async true
                          :effect (effect (runner-install eid card (assoc params :host-card target)))}
                         card nil)
       (wait-for (trigger-event-simult state side :pre-install nil card facedown)
                 (let [cost (runner-get-cost state side card params)]
                   (if (runner-can-install? state side card facedown)
                     (wait-for (pay-sync state side (make-eid state eid) card cost)
                               (if-let [cost-str async-result]
                                 (let [c (if host-card
                                           (host state side host-card card)
                                           (move state side card
                                                 [:rig (if facedown :facedown (to-keyword (:type card)))]))
                                       c (assoc c :installed :this-turn :new true)
                                       installed-card (if facedown
                                                        (do (update! state side c)
                                                            (find-latest state c))
                                                        (card-init state side c {:resolve-effect false
                                                                                 :init-data true}))]
                                   (when-not no-msg
                                     (runner-install-message state side (:title card) cost-str params))

                                   (play-sfx state side "install-runner")
                                   (when (and (program? card)
                                              (not facedown)
                                              (not no-mu))
                                     ;; Use up mu from program not installed facedown
                                     (use-mu state (:memoryunits card))
                                     (toast-check-mu state))
                                   (handle-virus-counter-flag state side installed-card)
                                   (when (and (not facedown) (resource? card))
                                     (swap! state assoc-in [:runner :register :installed-resource] true))
                                   (when (and (not facedown) (has-subtype? c "Icebreaker"))
                                     (update-breaker-strength state side c))
                                   (trigger-event-simult state side eid :runner-install
                                                         (when-not facedown
                                                           {:card-ability (card-as-handler installed-card)})
                                                         installed-card))
                                 (effect-completed state side eid)))
                               (effect-completed state side eid)))
                   (clear-install-cost-bonus state side)))
       (effect-completed state side eid))))
