(ns game.core.installing
  (:require
    [cond-plus.core :refer [cond+]]
    [game.core.agendas :refer [update-advancement-requirement]]
    [game.core.board :refer [all-installed get-remotes installable-servers server->zone all-installed-runner-type]]
    [game.core.card :refer [agenda? asset? convert-to-condition-counter corp? event? get-card get-zone has-subtype? ice? operation? program? resource? rezzed? installed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [ignore-install-cost? install-additional-cost-bonus install-cost]]
    [game.core.eid :refer [complete-with-result effect-completed eid-set-defaults make-eid]]
    [game.core.engine :refer [checkpoint register-pending-event pay queue-event register-events trigger-event-simult unregister-events]]
    [game.core.effects :refer [register-constant-effects unregister-constant-effects]]
    [game.core.flags :refer [turn-flag?]]
    [game.core.hosting :refer [host]]
    [game.core.ice :refer [update-breaker-strength]]
    [game.core.initializing :refer [ability-init card-init corp-ability-init runner-ability-init]]
    [game.core.memory :refer [sufficient-mu? update-mu]]
    [game.core.moving :refer [move trash trash-cards]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs]]
    [game.core.rezzing :refer [rez]]
    [game.core.say :refer [play-sfx system-msg implementation-msg]]
    [game.core.servers :refer [name-zone remote-num->name]]
    [game.core.state :refer [make-rid]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability effect req wait-for]]
    [game.utils :refer [dissoc-in in-coll? to-keyword]]))

(defn install-locked?
  "Checks if installing is locked"
  [state side]
  (let [kw (keyword (str (name side) "-lock-install"))]
    (or (seq (get-in @state [:stack :current-run kw]))
        (seq (get-in @state [:stack :current-turn kw]))
        (seq (get-in @state [:stack :persistent kw])))))

;;; Intalling a corp card
(defn- corp-can-install-reason
  "Checks if the specified card can be installed.
   Returns true if there are no problems
   Returns :region if Region check fails
   Returns :ice if ice check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card slot]
  (cond
    ;; Region check
    (and (has-subtype? card "Region")
         (some #(has-subtype? % "Region") (get-in @state (cons :corp slot))))
    :region
    ;; ice install prevented by Unscheduled Maintenance
    (and (ice? card)
         (not (turn-flag? state side card :can-install-ice)))
    :ice
    ;; Installing not locked
    (install-locked? state :corp)
    :lock-install
    ;; Earth station cannot have more than one server
    (and (= "Earth Station" (subs (:title (get-in @state [:corp :identity])) 0 (min 13 (count (:title (get-in @state [:corp :identity]))))))
         (not (:disabled (get-in @state [:corp :identity])))
         (pos? (count (get-remotes state)))
         (not (in-coll? (conj (keys (get-remotes state)) :archives :rd :hq) (second slot))))
    :earth-station
    ;; no restrictions
    :else true))

(defn- corp-can-install?
  "Checks `corp-can-install-reason` if not true, toasts reason and returns false"
  [state side card slot {:keys [no-toast]}]
  (let [reason (corp-can-install-reason state side card slot)
        reason-toast #(do (when-not no-toast (toast state side % "warning")) false)
        title (:title card)]
    (case reason
      ;; failed region check
      :region
      (reason-toast (str "Cannot install " (:title card) ", limit of one Region per server"))
      ;; failed install lock check
      :lock-install
      (reason-toast (str "Unable to install " title ", installing is currently locked"))
      ;; failed ice check
      :ice
      (reason-toast (str "Unable to install " title ": can only install 1 piece of ice per turn"))
      ;; Earth station cannot have more than one remote server
      :earth-station
      (reason-toast (str "Unable to install " title " in new remote: Earth Station limit"))
      ;; else
      true)))

(defn- corp-install-asset-agenda
  "Forces the corp to trash an existing asset or agenda if a second was just installed."
  [state side eid card dest-zone server]
  (let [prev-card (some #(when (or (asset? %) (agenda? %)) %) dest-zone)]
    (if (and (or (asset? card) (agenda? card))
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
                      (if (ice? card) "ice" "a card"))
          server-name (if (= server "New remote")
                        (str (remote-num->name (dec (:rid @state))) " (new remote)")
                        server)]
      (system-msg state side (str (build-spend-msg cost-str "install") card-name
                                  (if (ice? card) " protecting " " in ") server-name))
      (when (and (= :face-up install-state)
                 (agenda? card))
        (implementation-msg state card)))))

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
  [state side eid card server {:keys [install-state host-card front index display-message] :as args} slot cost-str]
  (let [cdef (card-def card)
        dest-zone (get-in @state (cons :corp slot))
        install-state (or (:install-state cdef) install-state)
        no-msg (not (if (nil? display-message) true display-message))
        c (-> card
              (assoc :advanceable (:advanceable cdef) :new true)
              (dissoc :seen :disabled))]
    (when-not host-card
      (corp-install-message state side c server install-state cost-str args))
    (play-sfx state side "install-corp")
    (let [moved-card (if host-card
                       (host state side host-card (assoc c :installed true))
                       (move state side c slot {:front front
                                                :index index}))
          _ (when (agenda? c)
              (update-advancement-requirement state moved-card))
          moved-card (get-card state moved-card)]
      ;; Check to see if a second agenda/asset was installed.
      (wait-for (corp-install-asset-agenda state side moved-card dest-zone server)
                (let [eid (assoc eid :source moved-card :source-type :rez)]
                  (queue-event state :corp-install {:card (get-card state moved-card)
                                                    :install-state install-state})
                  (case install-state
                    ;; Ignore all costs
                    :rezzed-no-cost
                    (if-not (agenda? moved-card)
                      (rez state side eid moved-card {:ignore-cost :all-costs
                                                      :no-msg no-msg})
                      (checkpoint state nil eid))
                    ;; Ignore rez cost only
                    :rezzed-no-rez-cost
                    (rez state side eid moved-card {:ignore-cost :rez-costs
                                                    :no-msg no-msg})
                    ;; Pay costs
                    :rezzed
                    (if-not (agenda? moved-card)
                      (rez state side eid moved-card {:no-msg no-msg})
                      (checkpoint state nil eid))
                    ;; "Face-up" cards
                    :face-up
                    (let [moved-card (-> (get-card state moved-card)
                                         (assoc :seen true)
                                         (cond-> (not (agenda? card)) (assoc :rezzed true)))
                          moved-card (if (:install-state cdef)
                                       (card-init state side moved-card {:resolve-effect false
                                                                         :init-data true})
                                       (update! state side moved-card))]
                      (wait-for (checkpoint state nil (make-eid state eid))
                                (complete-with-result state side eid (get-card state moved-card))))
                    ;; All other cards
                    (wait-for (checkpoint state nil (make-eid state eid))
                              (when-let [dre (:derezzed-events cdef)]
                                (register-events state side moved-card (map #(assoc % :condition :derezzed) dre)))
                              (complete-with-result state side eid (get-card state moved-card)))))))))

(defn get-slot
  [state card server {:keys [host-card]}]
  (if host-card
    (get-zone host-card)
    (conj (server->zone state server) (if (ice? card) :ices :content))))

(defn corp-install-cost
  [state side card server
   {:keys [base-cost ignore-install-cost ignore-all-cost cost-bonus cached-costs] :as args}]
  (or cached-costs
      (let [slot (get-slot state card server args)
            dest-zone (get-in @state (cons :corp slot))
            ice-cost (if (and (ice? card)
                              (not ignore-install-cost)
                              (not ignore-all-cost)
                              (not (ignore-install-cost? state side card)))
                       (count dest-zone)
                       0)
            cost (install-cost state side card
                               {:cost-bonus (+ (or cost-bonus 0) ice-cost)}
                               {:server server
                                :dest-zone dest-zone})]
        (when-not ignore-all-cost
          (merge-costs [base-cost [:credit cost]])))))

(defn corp-can-pay-and-install?
  [state side eid card server args]
  (let [slot (get-slot state card server (select-keys args [:host-card]))
        costs (corp-install-cost state side card server args)]
    (and (corp-can-install? state side card slot (select-keys args [:no-toast]))
         (can-pay? state side eid card nil costs)
         ;; explicitly return true
         true)))

(defn- corp-install-pay
  "Used by corp-install to pay install costs"
  [state side eid card server {:keys [action] :as args}]
  (let [slot (get-slot state card server args)
        costs (corp-install-cost state side card server (dissoc args :cached-costs))]
    (if (corp-can-pay-and-install? state side eid card server (assoc args :cached-costs costs))
      (wait-for (pay state side (make-eid state eid) card costs {:action action})
                (if-let [payment-str (:msg async-result)]
                  (if (= server "New remote")
                    (wait-for (trigger-event-simult state side :server-created nil card)
                              (make-rid state)
                              (corp-install-continue state side eid card server args slot payment-str))
                    (corp-install-continue state side eid card server args slot payment-str))
                  (effect-completed state side eid)))
      (effect-completed state side eid))))

(defn corp-install
  "Installs a card in the chosen server. If server is nil, asks for server to install in.
  The args input takes the following values:
  :base-cost - Only used for click actions
  :host-card - Card to host on
  :ignore-all-cost - true if install costs should be ignored
  :action - What type of action installed the card
  :install-state - Can be :rezzed-no-cost, :rezzed-no-rez-cost, :rezzed, or :face-up
  :display-message - Print descriptive text to the log window [default=true]
  :index - which position for an installed piece of ice"
  ([state side eid card server] (corp-install state side eid card server nil))
  ([state side eid card server {:keys [host-card] :as args}]
   (let [eid (eid-set-defaults eid :source nil :source-type :corp-install)]
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
       (and (map? server)
            (not host-card))
       (corp-install state side eid card server (assoc args :host-card server))
       ;; A server was selected
       :else
       (do (swap! state dissoc-in [:corp :install-list])
           (corp-install-pay state side eid card server args))))))

;; Unused in the corp install system, necessary for card definitions
(defn corp-install-msg
  "Gets a message describing where a card has been installed from. Example: Interns."
  [card]
  (str "install " (if (:seen card) (:title card) "an unseen card") " from " (name-zone :corp (:zone card))))

;;; Installing a runner card
(defn- runner-can-install-reason
  "Checks if the specified card can be installed.
   Checks uniqueness of card and installed console.
   Returns true if there are no problems
   Returns :req if card-def :req check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card facedown]
  (let [card-req (:req (card-def card))]
    (cond
      ;; Can always install a card facedown
      facedown true
      ;; Installing not locked
      (install-locked? state :runner) :lock-install
      ;; Req check
      (and card-req (not (card-req state side (make-eid state) card nil))) :req
      ;; The card's zone is locked
      (get-in @state [side :locked (first (get-zone card))]) :locked-zone
      ;; Nothing preventing install
      :else true)))

(defn runner-can-install?
  "Checks `runner-can-install-reason` if not true, toasts reason and returns false"
  ([state side card] (runner-can-install? state side card nil))
  ([state side card {:keys [facedown no-toast]}]
   (let [reason (runner-can-install-reason state side card facedown)
         reason-toast #(do (when-not no-toast (toast state side % "warning")) false)
         title (:title card)]
     (case reason
       :lock-install
       (reason-toast (str "Unable to install " title " since installing is currently locked"))
       :req
       (reason-toast (str "Installation requirements are not fulfilled for " title))
       :locked-zone
       (reason-toast (str "Unable to install " title " because it is currently in a locked zone"))
       ;; else
       true))))

(defn- runner-install-message
  "Prints the correct msg for the card install"
  [state side card-title cost-str
   {:keys [no-cost host-card facedown custom-message]}]
  (if facedown
    (system-msg state side "installs a card facedown")
    (if custom-message
      (system-msg state side (custom-message cost-str))
      (system-msg state side
                  (str (build-spend-msg cost-str "install") card-title
                       (when host-card (str " on " (card-str state host-card)))
                       (when no-cost " at no cost"))))))

(defn runner-install-continue
  [state side eid card
   {:keys [previous-zone host-card facedown no-mu no-msg payment-str] :as args}]
  (let [c (if host-card
            (host state side host-card card)
            (move state side card
                  [:rig (if facedown :facedown (to-keyword (:type card)))]))
        c (assoc c
                 :installed :this-turn
                 :new true
                 :previous-zone previous-zone)
        installed-card (if facedown
                         (update! state side c)
                         (card-init state side c {:resolve-effect false
                                                  :init-data true
                                                  :no-mu no-mu}))]
    (when-not no-msg
      (runner-install-message state side (:title installed-card) payment-str args))
    (when-not facedown
      (implementation-msg state card))
    (play-sfx state side "install-runner")
    (when (and (not facedown)
               (resource? card))
      (swap! state assoc-in [:runner :register :installed-resource] true))
    (when (and (not facedown)
               (has-subtype? installed-card "Icebreaker"))
      (update-breaker-strength state side installed-card))
    (queue-event state :runner-install {:card (get-card state installed-card)
                                        :facedown facedown})
    (when-let [on-install (and (not facedown)
                               (:on-install (card-def installed-card)))]
      (register-pending-event state :runner-install installed-card on-install))
    (wait-for (checkpoint state nil (make-eid state eid) nil)
              (complete-with-result state side eid (get-card state installed-card)))))

(defn- runner-install-cost
  "Get the total install cost for specified card"
  [state side card
   {:keys [base-cost ignore-install-cost ignore-all-cost facedown cost-bonus cached-costs]}]
  (cond+
    [cached-costs]
    [(or ignore-all-cost facedown) [:credit 0]]
    [:else
     (let [cost (install-cost state side card {:cost-bonus cost-bonus} {:facedown facedown})
           additional-costs (install-additional-cost-bonus state side card)]
       (merge-costs
         [base-cost
          (when (and (not ignore-install-cost)
                     (not facedown))
            [:credit cost])
          additional-costs]))]))

(defn runner-can-pay-and-install?
  [state side eid card {:keys [facedown] :as args}]
  (let [costs (runner-install-cost state side (assoc card :facedown facedown) args)]
    (and (runner-can-install? state side card args)
         (can-pay? state side eid card nil costs)
         ;; explicitly return true
         true)))

(defn runner-install-pay
  [state side eid card {:keys [facedown] :as args}]
  (let [costs (runner-install-cost state side (assoc card :facedown facedown) (dissoc args :cached-costs))]
    (if-not (runner-can-pay-and-install? state side eid card (assoc args :cached-costs costs))
      (effect-completed state side eid)
      (if (and (program? card)
               (not facedown)
               (not (sufficient-mu? state card)))
        (continue-ability
          state side
          {:prompt (format "Insufficient MU to install %s. Trash installed programs?" (:title card))
           :choices {:max (count (all-installed-runner-type state :program))
                     :card #(and (installed? %)
                                 (program? %))}
           :async true
           :effect (req (wait-for (trash-cards state side (make-eid state eid) targets {:game-trash true})
                                  (update-mu state)
                                  (runner-install-pay state side eid card args)))
           :cancel-effect (effect (effect-completed eid))}
          card nil)
        (let [played-card (move state side (assoc card :facedown facedown) :play-area {:suppress-event true})]
          (wait-for (pay state side (make-eid state eid) card costs)
                    (if-let [payment-str (:msg async-result)]
                      (runner-install-continue
                        state side eid
                        played-card (assoc args
                                           :previous-zone (:zone card)
                                           :payment-str payment-str))
                      (let [returned-card (move state :runner played-card (:zone card) {:suppress-event true})]
                        (update! state :runner
                                 (assoc returned-card
                                        :cid (:cid card)
                                        :previous-zone (:previous-zone card)))
                        (effect-completed state side eid)))))))))

(defn runner-install
  "Installs specified runner card if able"
  ([state side eid card] (runner-install state side eid card nil))
  ([state side eid card {:keys [host-card facedown] :as args}]
   (let [eid (eid-set-defaults eid :source nil :source-type :runner-install)
         hosting (and (not host-card)
                      (not facedown)
                      (:hosting (card-def card)))]
     (if hosting
       (continue-ability
         state side
         {:choices hosting
          :prompt (str "Choose a card to host " (:title card) " on")
          :async true
          :effect (effect (runner-install-pay eid card (assoc args :host-card target)))}
         card nil)
       (runner-install-pay state side eid card args)))))

(defn install-as-condition-counter
  "Install the event or operation onto the target as a condition counter."
  [state side eid card target]
  (assert (or (event? card) (operation? card)) "condition counter must be event or operation")
  (let [cdef (card-def card)
        abilities (ability-init cdef)
        corp-abilities (corp-ability-init cdef)
        runner-abilities (runner-ability-init cdef)
        card (convert-to-condition-counter card)
        events (filter #(= :hosted (:condition %)) (:events cdef))]
    (if (corp? card)
      (wait-for (corp-install state side (make-eid state eid)
                              card target {:host-card target
                                           :ignore-all-cost true})
                (let [card (update! state side (assoc async-result
                                                      :abilities abilities
                                                      :runner-abilities runner-abilities))]
                  (unregister-events state side card)
                  (unregister-constant-effects state side card)
                  (register-events state side card events)
                  (register-constant-effects state side card)
                  (complete-with-result state side eid card)))
      (wait-for (runner-install state side (make-eid state eid)
                                card {:host-card target
                                      :ignore-all-cost true})
                (let [card (update! state side (assoc async-result
                                                      :abilities abilities
                                                      :corp-abilities corp-abilities))]
                  (unregister-events state side card)
                  (unregister-constant-effects state side card)
                  (register-events state side card events)
                  (register-constant-effects state side card)
                  (complete-with-result state side eid card))))))
