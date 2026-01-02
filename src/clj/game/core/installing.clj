(ns game.core.installing
  (:require
    [cond-plus.core :refer [cond+]]
    [clojure.string :as string]
    [game.core.agendas :refer [update-advancement-requirement]]
    [game.core.board :refer [all-installed get-remotes installable-servers server->zone all-installed-runner-type]]
    [game.core.card :refer [agenda? asset? card-index condition-counter? convert-to-condition-counter  corp? event? get-card get-zone has-subtype? ice? installed? operation? program? resource? rezzed? upgrade?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [ignore-install-cost? install-additional-cost-bonus install-cost]]
    [game.core.costs :refer [total-available-credits]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid]]
    [game.core.engine :refer [checkpoint register-pending-event pay queue-event register-events trigger-event-simult unregister-events]]
    [game.core.effects :refer [is-disabled-reg? register-static-abilities unregister-static-abilities update-disabled-cards]]
    [game.core.flags :refer [turn-flag? zone-locked?]]
    [game.core.hosting :refer [has-ancestor? host]]
    [game.core.ice :refer [update-breaker-strength]]
    [game.core.initializing :refer [ability-init card-init corp-ability-init runner-ability-init]]
    [game.core.memory :refer [available-mu expected-mu sufficient-mu? update-mu]]
    [game.core.moving :refer [move trash trash-cards swap-cards swap-installed]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs ->c value]]
    [game.core.props :refer [add-prop]]
    [game.core.revealing :refer [reveal]]
    [game.core.rezzing :refer [rez]]
    [game.core.say :refer [play-sfx system-msg implementation-msg]]
    [game.core.servers :refer [name-zone remote-num->name]]
    [game.core.state :refer [make-rid]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability effect req wait-for]]
    [game.utils :refer [dissoc-in enumerate-str in-coll? same-card? to-keyword quantify]]
    [medley.core :refer [find-first]]))

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
   Returns :ice if ice check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card slot]
  (cond
    ;; ice install prevented by Unscheduled Maintenance
    (and (ice? card)
         (not (turn-flag? state side card :can-install-ice)))
    :ice
    ;; Installing not locked
    (install-locked? state :corp)
    :lock-install
    ;; A Teia cannot have more than two servers
    (and (clojure.string/starts-with? (:title (get-in @state [:corp :identity])) "A Teia")
         (not (:disabled (get-in @state [:corp :identity])))
         (not (is-disabled-reg? state (get-in @state [:corp :identity])))
         (<= 2 (count (get-remotes state)))
         (not (in-coll? (conj (keys (get-remotes state)) :archives :rd :hq) (second slot))))
    :a-teia
    ;; Earth station cannot have more than one server
    (and (= "Earth Station" (subs (:title (get-in @state [:corp :identity])) 0 (min 13 (count (:title (get-in @state [:corp :identity]))))))
         (not (:disabled (get-in @state [:corp :identity])))
         (not (is-disabled-reg? state (get-in @state [:corp :identity])))
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
      ;; failed install lock check
      :lock-install
      (reason-toast (str "Unable to install " title ", installing is currently locked"))
      ;; failed ice check
      :ice
      (reason-toast (str "Unable to install " title ": can only install 1 piece of ice per turn"))
      ;; Earth station cannot have more than one remote server
      :earth-station
      (reason-toast (str "Unable to install " title " in new remote: Earth Station limit"))
      ;; A Teia can only have two remotes
      :a-teia
      (reason-toast (str "Unable to install " title " in new remote: A Teia limit"))
      ;; else
      true)))

(defn- corp-install-trash-old-card
  "Trashes the previous card when installing a new one demands it"
  [state side eid prev-card server]
  (continue-ability
    state side
    {:prompt (str "The " (:title prev-card) " in " server " will now be trashed.")
     :choices ["OK"]
     :async true
     :effect (req (system-msg state :corp (str "trashes " (card-str state prev-card)))
                  (if (get-card state prev-card) ; make sure they didn't trash the card themselves
                    (trash state :corp eid prev-card {:keep-server-alive true :suppress-checkpoint true :during-installation true})
                    (effect-completed state :corp eid)))}
    nil nil))

(defn- corp-install-place-counters
  "Places counters on a card via installation"
  [state side eid target-card {:keys [counters] :as args}]
  ;; for now, only advancement counters are checked
  (if (:advance-counter counters)
    (add-prop state side eid target-card :advance-counter (:advance-counter counters) {:placed true})
    (effect-completed state side eid)))

(defn- corp-install-asset-agenda
  "Forces the corp to trash an existing asset or agenda if a second was just installed."
  [state side eid card dest-zone server]
  (let [prev-card (some #(when (or (asset? %) (agenda? %)) %) dest-zone)
        prev-region (first (filter #(has-subtype? % "Region") dest-zone))]
    (cond
      ;; overinstall an old asset or agenda
      (and (or (asset? card) (agenda? card))
                 prev-card
                 (not (:host card)))
      (corp-install-trash-old-card state side eid prev-card server)
      ;; overinstall a region
      (and (upgrade? card)
           (has-subtype? card "Region")
           prev-region)
      (corp-install-trash-old-card state side eid prev-region server)
      ;; do nothing
      :else (effect-completed state side eid))))

(defn- format-counters-msg
  [{:keys [advance-counter] :as counters}]
  ;; TODO - rewrite this if/when we support more counter types through installs
  (if advance-counter
    (str ", and place " (quantify advance-counter "Advancement counter") " on it")
    ""))

(defn- corp-install-message
  "Prints the correct install message."
  [state side card server install-state cost-str {:keys [counters msg-keys] :as args}]
  (when (:display-message args true)
    (let [{:keys [display-origin install-source origin-index known set-zone]} msg-keys
          prepend-cost-str (get-in msg-keys [:include-cost-from-eid :latest-payment-str])
          card-name (if (or (#{:rezzed :rezzed-no-cost :face-up} install-state)
                            ;; note that cards which the corp is instructed to rez, but cannot
                            ;; (or chooses not to rez) are revealed, so they're safe to name here
                            known
                            (:seen card)
                            (rezzed? card))
                      (:title card)
                      (if (ice? card) "ice" "a card"))
          server-name (if (= server "New remote")
                        (str (remote-num->name (dec (:rid @state))) " (new remote)")
                        server)
          origin (if display-origin
                   (str " from "
                        (when origin-index (str " position " (inc origin-index) " of "))
                        (or set-zone (name-zone :corp (:zone card))))
                   "")
          pre-lhs (when (every? (complement string/blank?) [cost-str prepend-cost-str])
                    (str prepend-cost-str ", and then "))
          modified-cost-str (if (string/blank? cost-str)
                              prepend-cost-str
                              (if (string/blank? pre-lhs)
                                cost-str
                                (str cost-str ",")))
          lhs (if install-source
                (str (build-spend-msg modified-cost-str "use") (:title install-source) " to install ")
                (build-spend-msg modified-cost-str "install"))]
      (system-msg state side (str lhs card-name origin
                                  (if (ice? card) " protecting " " in the root of ") server-name
                                  (format-counters-msg counters)))
      (when (and (= :face-up install-state)
                 (agenda? card))
        (implementation-msg state card)))))

;; Unused in the corp install system, necessary for card definitions
(defn corp-install-msg
  "Gets a message describing where a card has been installed from. Example: Interns."
  [card]
  (str "install " (if (:seen card) (:title card) "an unseen card") " from " (name-zone :corp (:zone card))))

(defn reveal-if-unrezzed
  "Used to reveal a card if it cannot be rezzed when an instruction says to rez it
  This is currently required under CR, and is treated as an actual functional reveal
  for the purposes of card effects (ie Hyobou). It does not matter if that would then
  make you able to rez the chosen card."
  [state side eid moved-card]
  (let [rezzed-card (get-card state moved-card)]
    (if (rezzed? rezzed-card)
      (wait-for (checkpoint state nil)
                (complete-with-result state side eid (get-card state moved-card)))
      (wait-for (reveal state :corp rezzed-card)
                (system-msg state :corp (str "reveals " (card-str state rezzed-card {:visible true})))
                (wait-for (checkpoint state nil)
                          (complete-with-result state side eid (get-card state moved-card)))))))

(defn- corp-install-continue
  "Used by corp-install to actually install the card, rez it if it's supposed to be installed
  rezzed, and calls :corp-install in an awaitable fashion."
  [state side eid card server {:keys [install-state host-card front index display-message cost-bonus] :as args} slot cost-str]
  (let [cdef (card-def card)
        dest-zone (get-in @state (cons :corp slot))
        install-state (or (:install-state cdef) install-state)
        no-msg (not (if (nil? display-message) true display-message))
        ;; if cards are no longer candidates, but get installed during a breach, it is known
        from-zone (or (#{:discard :deck :hand} (first (get-zone card)))
                      (second (get-zone card)))
        args (update-in args [:msg-keys :known] #(or % (contains? (set (get-in @state [:breach :known-cids from-zone] [])) (:cid card))))
        c (-> card
              (assoc :advanceable (:advanceable cdef) :new true)
              (dissoc :seen :disabled))]
    (when-not host-card
      (corp-install-message state side card server install-state cost-str args))
    (play-sfx state side "install-corp")
    (let [moved-card (if host-card
                       (host state side host-card (assoc c :installed true))
                       (move state side c slot {:front front
                                                :index index}))
          _ (when (agenda? c)
              (update-advancement-requirement state moved-card))
          moved-card (get-card state moved-card)]
      (unregister-events state side moved-card)
      ;; Check to see if a second agenda/asset was installed.
      (wait-for
        (corp-install-asset-agenda state side moved-card dest-zone server)
        (wait-for
          (corp-install-place-counters state side moved-card args)
          (let [moved-card (get-card state moved-card)
                eid (assoc eid :source moved-card)]
            (queue-event state :corp-install {:card (get-card state moved-card)
                                              :install-state install-state})
            (when-let [dre (:derezzed-events cdef)]
              (register-events state side moved-card (map #(assoc % :condition :derezzed) dre)))
            (update-disabled-cards state)
            (when (:breach @state)
              (swap! state update-in [:breach :installed] (fnil conj #{}) (:cid moved-card)))
            (case install-state
              ;; Ignore all costs
              :rezzed-no-cost
              (wait-for
                (checkpoint state nil (make-eid state eid))
                (if-not (agenda? moved-card)
                  (rez state side (assoc eid :source-type :rez :source (-> args :msg-keys :install-source))
                       moved-card {:ignore-cost :all-costs
                                   :no-msg no-msg})
                  (reveal-if-unrezzed state side eid moved-card)))
              ;; Ignore rez cost only
              :rezzed-no-rez-cost
              (wait-for
                (checkpoint state nil (make-eid state eid))
                (wait-for (rez state side (make-eid state (assoc eid :source-type :rez))
                               moved-card {:ignore-cost :rez-costs
                                           :no-msg no-msg})
                          (reveal-if-unrezzed state side eid moved-card)))
              ;; Pay costs
              :rezzed
              (let [eid (assoc eid :source-type :rez)]
                (cond
                  (agenda? moved-card)
                  (reveal-if-unrezzed state side eid moved-card)
                  (zero? cost-bonus)
                  (wait-for
                    (checkpoint state nil (make-eid state eid))
                    (wait-for
                      (rez state side moved-card {:no-msg no-msg})
                      (reveal-if-unrezzed state side eid moved-card)))
                  :else
                  (wait-for
                    (checkpoint state nil (make-eid state eid))
                    (wait-for
                      (rez state side moved-card {:no-msg no-msg
                                                  :cost-bonus cost-bonus})
                      (reveal-if-unrezzed state side eid moved-card)))))
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
                        (complete-with-result state side eid (get-card state moved-card))))))))))

(defn get-slot
  [state card server {:keys [host-card]}]
  (if host-card
    (get-zone host-card)
    (conj (server->zone state server) (if (ice? card) :ices :content))))

(defn corp-install-cost
  [state side card server
   {:keys [base-cost ignore-install-cost ignore-all-cost cost-bonus cached-costs ignore-ice-cost] :as args}]
  (or cached-costs
      (let [slot (get-slot state card server args)
            dest-zone (get-in @state (cons :corp slot))
            ice-cost (if (and (ice? card)
                              (not ignore-install-cost)
                              (not ignore-all-cost)
                              (not ignore-ice-cost)
                              (not (ignore-install-cost? state side card)))
                       (count dest-zone)
                       0)
            cost (install-cost state side card
                               {:cost-bonus (+ (or cost-bonus 0) ice-cost)}
                               {:server server
                                :dest-zone dest-zone})]
        (when-not ignore-all-cost
          (vec (flatten [base-cost (->c :credit cost)]))))))

(defn corp-can-pay-and-install?
  [state side eid card server args]
  (let [eid (assoc eid :source-type :corp-install)
        slot (get-slot state card server (select-keys args [:host-card]))
        costs (corp-install-cost state side card server args)]
    (and (corp-can-install? state side card slot (select-keys args [:no-toast]))
         (can-pay? state side eid card nil costs)
         ;; explicitly return true
         true)))

(defn- corp-install-pay
  "Used by corp-install to pay install costs"
  [state side eid card server {:keys [action resolved-optional-trash] :as args}]
  (let [slot (get-slot state card server args)
        costs (corp-install-cost state side card server (dissoc args :cached-costs))
        credcost (or (value (find-first #(= :credit (:cost/type %)) costs)) 0)
        discount (or (:combined-credit-discount args) 0)
        appldisc (if (and (not (zero? credcost)) (not (zero? discount)))
                   (if (>= credcost discount) discount credcost) 0)
        args (if discount (assoc args :cost-bonus (- appldisc discount)) args)
        costs (conj costs (->c :credit (- 0 appldisc)))
        corp-wants-to-trash? (and (get-in @state [:corp :properties :trash-like-cards])
                                  (seq (get-in @state (concat [:corp] slot)))
                                  (not resolved-optional-trash))]
    (if (and (not corp-wants-to-trash?) (corp-can-pay-and-install? state side eid card server (assoc args :cached-costs costs)))
      (wait-for
        (pay state side (make-eid state (assoc eid :action action)) card costs)
        (if-let [payment-str (:msg async-result)]
          (if (= server "New remote")
            (wait-for (trigger-event-simult state side :server-created nil card)
                      (make-rid state)
                      (corp-install-continue state side eid card server args slot payment-str))
            (corp-install-continue state side eid card server args slot payment-str))
          (effect-completed state side eid)))
      ;; NOTE - Diwan and Network Exchange both alter the cost of installs
      ;; if it's not ice AND we can't afford it, there's nothing we can do
      ;; Diwan will get accounted for, but Network Exchange wont (oh well) - nbk, 2025
      (let [shortfall (- (or (value (find-first #(= :credit (:cost/type %)) costs)) 0) (total-available-credits state side eid card))
            need-to-trash (max 0 shortfall)
            cards-in-slot (count (get-in @state (concat [:corp] slot)))
            possible? (and (ice? card) (>= cards-in-slot need-to-trash))
            c card]
        (cond (and possible? (pos? need-to-trash))
              (letfn [(trash-all-or-none [] {:prompt (str "Trash ice protecting " (name-zone :corp slot) " (minimum " need-to-trash ")")
                                             :choices {:req (req (= (:zone target) slot))
                                                       :max cards-in-slot}
                                             :waiting-prompt true
                                             :async true
                                             :effect (req (if (>= (count targets) need-to-trash)
                                                            (do (system-msg state side (str "trashes " (enumerate-str (map #(card-str state %) targets))))
                                                                (wait-for
                                                                  (trash-cards state side targets {:keep-server-alive true :suppress-checkpoint true :during-installation true})
                                                                  (corp-install-pay state side eid c server (assoc args :resolved-optional-trash true))))
                                                            (do (toast state :corp (str "You must either trash at least " need-to-trash " ice, or trash none of them"))
                                                                (continue-ability state side (trash-all-or-none) c targets))))
                                             :cancel-effect (req (effect-completed state side eid))})]
                (continue-ability state side (trash-all-or-none) nil nil))
              (and corp-wants-to-trash? (zero? need-to-trash))
              (continue-ability
                state side
                {:prompt (str "Trash any number of " (if (ice? c) "ice protecting " "cards in ") (name-zone :corp slot))
                 :choices {:req (req (= (:zone target) slot))
                           :max cards-in-slot}
                 :async true
                 :waiting-prompt true
                 :effect (req (do (system-msg state side (str "trashes " (enumerate-str (map #(card-str state %) targets))))
                                  (wait-for
                                    (trash-cards state side targets {:keep-server-alive true :suppress-checkpoint true :during-installation true})
                                    (corp-install-pay state side eid c server (assoc args :resolved-optional-trash true)))))
                 :cancel-effect (req (corp-install-pay state side eid c server (assoc args :resolved-optional-trash true)))}
                nil nil)
              :else (effect-completed state side eid))))))

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
   (let [eid (assoc eid :source-type :corp-install)]
     (cond
       ;; No server selected; show prompt to select an install site (Interns, Lateral Growth, etc.)
       (not server)
       (continue-ability state side
                         {:prompt (str "Choose a location to install " (:title card))
                          :choices (installable-servers state card)
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

;;; Installing a runner card
(defn- card-has-a-valid-host?
  "Checks if the specified card has a valid host (in the cases where it needs one)"
  [state side eid card facedown]
  (or facedown
      (if-let [host-req (->> card card-def :hosting :req)]
        (some #(host-req state side eid card [%]) (concat (all-installed state :corp) (all-installed state :runner)))
        true)))

(defn- runner-can-install-reason
  "Checks if the specified card can be installed.
   Checks uniqueness of card and installed console.
   Returns true if there are no problems
   Returns :req if card-def :req check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side eid card facedown]
  (let [card-req (:req (card-def card))]
    (cond
      ;; Can always install a card facedown
      facedown true
      ;; Installing not locked
      (install-locked? state :runner) :lock-install
      ;; Req check
      (and card-req (not (card-req state side eid card nil))) :req
      ;; if the card requires a host, there is a valid host
      (not (card-has-a-valid-host? state side eid card facedown)) :no-valid-host
      ;; The card's zone is locked
      (zone-locked? state side (first (get-zone card))) :locked-zone
      ;; Nothing preventing install
      :else true)))

(defn runner-can-install?
  "Checks `runner-can-install-reason` if not true, toasts reason and returns false"
  ([state side eid card] (runner-can-install? state side eid card nil))
  ([state side eid card {:keys [facedown no-toast]}]
   (let [reason (runner-can-install-reason state side eid card facedown)
         reason-toast #(do (when-not no-toast (toast state side % "warning")) false)
         title (:title card)]
     (case reason
       :lock-install
       (reason-toast (str "Unable to install " title " since installing is currently locked"))
       :req
       (reason-toast (str "Installation requirements are not fulfilled for " title))
       :no-valid-host
       (reason-toast (str "There is no valid host for " title))
       :locked-zone
       (reason-toast (str "Unable to install " title " because it is currently in a locked zone"))
       ;; else
       true))))

(defn- runner-install-message
  "Prints the correct msg for the card install"
  [state side card cost-str
   {:keys [no-cost host-card facedown custom-message msg-keys ignore-install-cost ignore-all-cost cost-bonus] :as args}]
  (let [{:keys [display-origin install-source origin-index known]} msg-keys
        hide-zero-cost (:hide-zero-cost msg-keys facedown)
        cost-str (if (and hide-zero-cost (= cost-str "pays 0 [Credits]")) nil cost-str)
        prepend-cost-str (get-in msg-keys [:include-cost-from-eid :latest-payment-str])
        display-origin (or (when-not (contains? msg-keys :display-origin)
                             (not= (:previous-zone card) [:hand]))
                           display-origin)
        discount-str (cond
                       ignore-all-cost " (ignoring all costs)"
                       ignore-install-cost " (ignoring it's install cost)"
                       (and cost-bonus (pos? cost-bonus)) (str " (paying " cost-bonus " [Credits] more)")
                       (and cost-bonus (neg? cost-bonus)) (str " (paying " (* -1 cost-bonus) " [Credits] less)")
                       :else nil)
        card-name (if facedown
                    (if known
                      (str (:title card) " as a facedown card")
                      "a card facedown")
                    (:title card))
        origin (if (and display-origin (not= (:previous-zone card) [:onhost]))
                 (str " from "
                      (when origin-index (str " position " (inc origin-index) " of "))
                      (cond
                        (= (:previous-zone card) [:set-aside])
                        "among the set-aside cards"
                        :else
                        (name-zone :runner (:previous-zone card))))
                 "")
        pre-lhs (when (every? (complement string/blank?) [cost-str prepend-cost-str])
                  (str prepend-cost-str ", and then "))
        from-host? (when (and display-origin (= (:previous-zone card) [:onhost]))
                     "hosted ")
        modified-cost-str (if (string/blank? cost-str)
                            prepend-cost-str
                            (if (string/blank? pre-lhs)
                              cost-str
                              (str cost-str ",")))
        lhs (if install-source
              (str (build-spend-msg modified-cost-str "use") (:title install-source) " to install ")
              (build-spend-msg modified-cost-str "install"))]
    (when (:display-message args true)
      (if custom-message
        (system-msg state side (custom-message cost-str))
        (system-msg state side
                    (str pre-lhs lhs from-host? card-name origin discount-str
                         (when host-card (str " on " (card-str state host-card)))
                         (when no-cost " at no cost")))))))

(defn runner-install-continue
  [state side eid card
   {:keys [previous-zone host-card facedown no-mu no-msg payment-str costs] :as args}]
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
      (runner-install-message state side installed-card payment-str args))
    (when-not facedown
      (implementation-msg state card))
    (if-let [install-sound (when-not facedown (:install-sound (card-def card)))]
      (play-sfx state side install-sound)
      (play-sfx state side "install-runner"))
    (update-disabled-cards state)
    (when (and (not facedown)
               (resource? card))
      (swap! state assoc-in [:runner :register :installed-resource] true))
    (when (and (not facedown)
               (has-subtype? installed-card "Icebreaker"))
      (update-breaker-strength state side installed-card))
    (queue-event state :runner-install {:card (get-card state installed-card)
                                        :costs costs
                                        :facedown facedown})
    (when-let [on-install (and (not facedown)
                               (:on-install (card-def installed-card)))]
      (register-pending-event state :runner-install installed-card on-install))
    (wait-for (checkpoint state nil (make-eid state) nil)
              (complete-with-result state side eid (get-card state installed-card)))))

(defn- runner-install-cost
  "Get the total install cost for specified card"
  [state side card
   {:keys [base-cost ignore-install-cost ignore-all-cost facedown cost-bonus cached-costs]}]
  (cond+
    [cached-costs]
    [(or ignore-all-cost facedown) [(->c :credit 0)]]
    [:else
     (let [cost (install-cost state side card {:cost-bonus cost-bonus} {:facedown facedown})
           additional-costs (install-additional-cost-bonus state side card)]
       (merge-costs
         [base-cost
          (when (and (not ignore-install-cost)
                     (not facedown))
            (->c :credit cost))
          additional-costs]))]))

(defn runner-can-pay-and-install?
  ([state side eid card] (runner-can-pay-and-install? state side eid card nil))
  ([state side eid card {:keys [facedown] :as args}]
   (let [eid (assoc eid :source-type :runner-install)
         costs (runner-install-cost state side (assoc card :facedown facedown) args)]
     (and (runner-can-install? state side eid card (assoc args :no-toast true))
          (can-pay? state side eid card nil costs)
          ;; explicitly return true
          true))))

(defn runner-install-pay
  [state side eid card {:keys [no-mu facedown host-card resolved-optional-trash] :as args}]
  (let [costs (runner-install-cost state side (assoc card :facedown facedown) (dissoc args :cached-costs))
        available-mem (available-mu state)
        runner-wants-to-trash? (and (get-in @state [:runner :properties :trash-like-cards])
                                    (not resolved-optional-trash))]
    (if-not (runner-can-pay-and-install? state side eid card (assoc args :cached-costs costs))
      (effect-completed state side eid)
      (if (and (program? card)
               (not facedown)
               (or (not (or no-mu (sufficient-mu? state card)))
                   runner-wants-to-trash?))
        (continue-ability
          state side
          {:prompt (if (and runner-wants-to-trash? (or no-mu (sufficient-mu? state card)))
                     (format "Trash installed programs before installing %s?" (:title card))
                     (format "Insufficient MU to install %s. Trash installed programs?" (:title card)))
           :choices {:max (count (filter #(and (program? %) (not (has-ancestor? % host-card))) (all-installed state :runner)))
                     :card #(and (installed? %)
                                 ;; note: rules team says we can't create illegal gamestates by
                                 ;; trashing a host when installing a card. If they ever change it,
                                 ;; then be aware that the engine will hang when trying to do this
                                 ;; without these guards. - nbkelly, oct 2024
                                 (not (has-ancestor? % host-card))
                                 (program? %))}
           :async true
           :effect (req (wait-for (trash-cards state side (make-eid state eid) targets {:unpreventable true :suppress-checkpoint true})
                                  (update-mu state)
                                  (runner-install-pay state side eid card (assoc args :resolved-optional-trash true))))
           :cancel-effect (req (update-mu state)
                               (if (and (= available-mem (available-mu state))
                                        ;;(not runner-wants-to-trash?)
                                        (not (or no-mu (sufficient-mu? state card))))
                                 (effect-completed state side eid)
                                 (runner-install-pay state side eid card (assoc args :resolved-optional-trash true))))}
          card nil)
        (let [played-card (move state side (assoc card :facedown facedown) :play-area {:suppress-event true})]
          (wait-for (pay state side (make-eid state eid) card costs)
                    (if-let [payment-str (:msg async-result)]
                      (runner-install-continue
                        state side eid
                        played-card (assoc args
                                           :costs costs
                                           :previous-zone (:zone card)
                                           :payment-str payment-str))
                      (let [returned-card (move state :runner played-card (:zone card) {:suppress-event true})]
                        (update! state :runner
                                 (assoc returned-card
                                        :cid (:cid card)
                                        :previous-zone (:previous-zone card)))
                        (effect-completed state side eid)))))))))

(defn- some-hosting-effect
  [state card]
  "Gets the first (only) host effect of a card, if it exists and is not disabled"
  (when-not (is-disabled-reg? state card)
    (first (filter #(= (:type %) :can-host) (:static-abilities (card-def card))))))

(defn runner-can-host
  [state side eid card {:keys [host-card facedown] :as args}]
  "Gets a list of all cards that the runner can host the install target on"
  (when-not (or host-card facedown)
    (let [all-hosts (filter #(some-hosting-effect state %) (all-installed state :runner))
          relevant (filter #(let [ab (some-hosting-effect state %)]
                              (or (nil? (:req ab))
                                  ((:req ab) state side eid % [card])))
                           all-hosts)]
      (seq relevant))))

(defn runner-host-enforce-specific-memory
  [state side eid card potential-host args]
  "enforces limits on the total MU a host can support during install"
  (if-let [max-mu (when (program? card) (:max-mu (some-hosting-effect state potential-host)))]
    (let [max-mu (if (fn? max-mu)
                   (max-mu state side eid potential-host nil)
                   max-mu)
          relevant-cards (filter program? (:hosted potential-host))
          current-mu-host (reduce + 0 (map #(expected-mu state %) relevant-cards))
          card-mu (expected-mu state card)
          new-mu (+ card-mu current-mu-host)
          to-eliminate (- new-mu max-mu)]
      (if (pos? to-eliminate)
        (continue-ability
          state side
          {:prompt (str (:title potential-host) " can only handle " max-mu " MU of programs - trash programs on " (:title card) " worth at least " to-eliminate " MU")
           :choices {:req (req (and (program? target)
                                    (some #(same-card? % target) relevant-cards)))
                     :max (count relevant-cards)
                     :min 1}
           :async true
           ;; note - this is recursive because there's no good way to specify in the prompt that
           ;; the total selection is worth X memory, since the req function must be satisfied at
           ;; every point of the selection --nbkelly, jun 2024
           :effect (req (wait-for
                          (trash-cards state side (make-eid state eid) targets {:unpreventable true :suppress-checkpoint true})
                          (update-mu state)
                          (runner-host-enforce-specific-memory state side eid card
                                                               (get-card state potential-host)
                                                               args)))}
          card nil)
        (runner-install-pay state side eid card (assoc args :host-card potential-host))))
    (runner-install-pay state side eid card (assoc args :host-card potential-host))))

(defn runner-host-enforce-card-limits
  [state side eid card potential-host args]
  "Enforces limits on the number of hosted cards a host can have during install"
  (if-let [max-cards (:max-cards (some-hosting-effect state potential-host))]
    (let [max-cards (if (int? max-cards)
                      max-cards
                      (max-cards state side eid potential-host nil))
          relevant-cards (filter (complement condition-counter?) (:hosted potential-host))
          new-total (+ 1 (count relevant-cards))
          to-destroy (- new-total max-cards)]
      (if (pos? to-destroy)
        (continue-ability
          state side
          {:prompt (str "Insufficient Space - Choose at least " (quantify to-destroy "card") " on " (:title potential-host) " to trash")
           :choices {:req (req (some #(same-card? % target) relevant-cards))
                     :min to-destroy
                     :max (count relevant-cards)}
           :async true
           :effect (req (wait-for (trash-cards state side (make-eid state eid) targets {:unpreventable true :suppress-checkpoint true})
                                  (update-mu state)
                                  (runner-host-enforce-specific-memory state side eid card
                                                                       (get-card state potential-host) args)))}
          card nil)
        (runner-host-enforce-specific-memory state side eid card potential-host args)))
    (runner-host-enforce-specific-memory state side eid card potential-host args)))

(defn runner-host-choice
  [state side eid card potential-hosts args]
  "Have the runner choose where they are hosting the given card"
  (continue-ability
    state side
    {:choices (conj potential-hosts "The Rig")
     :prompt (str "Choose a destination for " (:title card))
     :async true
     :effect (req (if (= target "The Rig")
                    (runner-install-pay state side eid card args)
                    ;; todo - apply all the modifiers from the host map
                    (let [host-abi (some-hosting-effect state target)
                          old-cost-bonus (or (:cost-bonus args) 0)
                          new-cost-bonus (or (:cost-bonus host-abi) 0)
                          combined-cost-bonus (+ old-cost-bonus new-cost-bonus)
                          cost-bonus (if (zero? combined-cost-bonus) nil combined-cost-bonus)]
                      (runner-host-enforce-card-limits
                        state side eid card (get-card state target)
                        (assoc args
                               :no-mu (:no-mu host-abi)
                               :cost-bonus cost-bonus)))))}
    card nil))

(defn runner-install
  "Installs specified runner card if able"
  ([state side eid card] (runner-install state side eid card nil))
  ([state side eid card {:keys [host-card facedown] :as args}]
   (let [eid (assoc eid :source-type :runner-install)
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
       (if-let [potential-hosts (runner-can-host state side eid card args)]
         (runner-host-choice state side eid card potential-hosts args)
         (runner-install-pay state side eid card args))))))

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
                  (unregister-static-abilities state side card)
                  (register-events state side card events)
                  (register-static-abilities state side card)
                  (complete-with-result state side eid card)))
      (wait-for (runner-install state side (make-eid state eid)
                                card {:host-card target
                                      :ignore-all-cost true})
                (let [card (update! state side (assoc async-result
                                                      :abilities abilities
                                                      :corp-abilities corp-abilities))]
                  (unregister-events state side card)
                  (unregister-static-abilities state side card)
                  (register-events state side card events)
                  (register-static-abilities state side card)
                  (complete-with-result state side eid card))))))

(defn swap-cards-async
  "Swaps two cards when one or both aren't installed"
  [state side eid a b]
  (if (= side :corp)
    (let [async-result (swap-cards state side a b)
          moved-a (first async-result)
          moved-b (second async-result)
          install-event (= 1 (count (filter installed? [moved-a moved-b])))]
      ;; todo - we might need behaviour for runner swap installs down the line, depending on future cards
      ;; that's a problem for another day
      (if install-event
        (let [installed-card (if (installed? moved-a) moved-a moved-b)
              cdef (card-def installed-card)]
          (queue-event state :corp-install {:card (get-card state installed-card)
                                            :install-state (:install-state cdef)})
          (wait-for (checkpoint state nil (make-eid state eid))
                    (complete-with-result state side eid async-result)))
        (complete-with-result state side eid async-result)))
    ;; runner side
    (case (count (filter installed? [a b]))
      0 (complete-with-result state side eid (swap-cards state side a b))
      1 (let [old-installed (first (filter installed? [a b]))
              to-install (first (filter (complement installed?) [a b]))
              moved-a (move state side old-installed (get-zone to-install) {:index (card-index state to-install)
                                                                            :suppress-event true
                                                                            :swap true})
              install-args {:previous-zone (:zone to-install)
                            :host-card (when-let [host (:host old-installed)]
                                         (get-card state host))
                            :no-mu (when-let [host (:host old-installed)]
                                     (:no-mu (some-hosting-effect state host)))
                            :no-msg true}]
          (wait-for
            (runner-install-continue state side to-install install-args)
            (complete-with-result state side eid [async-result moved-a])))
      ;; this should not be possible
      2 (complete-with-result state side eid nil))))
