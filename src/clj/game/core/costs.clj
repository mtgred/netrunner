(in-ns 'game.core)

(declare forfeit prompt! toast damage mill installed? is-type? is-scored? system-msg facedown? make-result)

(defn deduce
  "Deduct the value from the player's attribute."
  [state side [attr value]]
  (swap! state update-in [side attr] (if (or (= attr :memory)
                                             (= attr :agenda-point)
                                             (= attr :hand-size-modification))
                                       ;; Memory, agenda points or hand size mod may be negative
                                       #(- % value)
                                       #(max 0 (- % value))))
  (when (and (= attr :credit) (= side :runner) (get-in @state [:runner :run-credit]))
    (swap! state update-in [:runner :run-credit] #(max 0 (- % value))))
  (when-let [cost-name (cost-names value attr)]
    cost-name))

(defn toast-msg-helper
  "Creates a toast message for given cost and title if applicable"
  [state side cost]
  (let [type (first cost)
        amount (last cost)]
    (when-not (or (some #(= type %) [:memory :net-damage])
                  (and (= type :forfeit) (>= (- (count (get-in @state [side :scored])) amount) 0))
                  (and (= type :mill) (>= (- (count (get-in @state [side :deck])) amount) 0))
                  (and (= type :tag) (>= (- (get-in @state [:runner :tag]) amount) 0))
                  (and (= type :ice) (>= (- (count (filter (every-pred rezzed? ice?) (all-installed state :corp))) amount) 0))
                  (and (= type :hardware) (>= (- (count (get-in @state [:runner :rig :hardware])) amount) 0))
                  (and (= type :program) (>= (- (count (get-in @state [:runner :rig :program])) amount) 0))
                  (and (= type :connection) (>= (- (count (filter #(has-subtype? % "Connection")
                                                                  (all-installed state :runner))) amount) 0))
                  (and (= type :shuffle-installed-to-stack) (>= (- (count (all-installed state :runner)) amount) 0))
                  (>= (- (or (get-in @state [side type]) -1 ) amount) 0))
      "Unable to pay")))

(defn can-pay?
  "Returns false if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  [state side title & args]
  (let [costs (merge-costs (remove #(or (nil? %) (map? %)) args))
        cost-msg (or (some #(toast-msg-helper state side %) costs))]
    ;; no cost message - hence can pay
    (if-not cost-msg
      costs
      (when title (toast state side (str cost-msg " for " title ".")) false))))

(defn pay-forfeit
  "Forfeit agenda as part of paying for a card or ability
  Amount is always 1 but can be extend if we ever need more than a single forfeit"
  ;; If multiples needed in future likely prompt-select needs work to take a function
  ;; instead of an ability
  [state side eid card n]
  (let [cost-name (cost-names n :forfeit)]
    (continue-ability state side
                    {:prompt "Choose an Agenda to forfeit"
                     :delayed-completion true
                     :choices {:max n
                               :req #(is-scored? state side %)}
                     :effect (req (when-completed (forfeit state side target)
                                                  (effect-completed state side (make-result eid cost-name))))}
                    card nil)
    cost-name))

(defn pay-trash
  "Trash a card as part of paying for a card or ability"
  ;; If multiples needed in future likely prompt-select needs work to take a function
  ;; instead of an ability
  ([state side eid card type amount select-fn] (pay-trash state side eid card type amount select-fn nil))
  ([state side eid card type amount select-fn args]
   (let [cost-name (cost-names amount type)]
     (continue-ability state side
                       {:prompt (str "Choose a " type "to trash")
                        :choices {:max amount
                                  :req select-fn}
                        :delayed-completion true
                        :effect (req (when-completed (trash state side target (merge args {:unpreventable true}))
                                                     (effect-completed state side (make-result eid cost-name))))}
                       card nil)
     cost-name)))

(defn pay-shuffle-installed-to-stack
  "Shuffle installed runner card(s) into the stack as part of paying for a card or ability"
  [state side eid card amount]
  (let [cost-name (cost-names amount :shuffle-installed-to-stack)]
    (continue-ability state :runner
                    {:prompt (str "Choose " amount " " (pluralize "card" amount) " to shuffle into the stack")
                     :choices {:max amount
                               :all true
                               :req #(and (installed? %) (= (:side %) "Runner"))}
                     :delayed-completion true
                     :effect (req
                               (doseq [c targets]
                                 (move state :runner c :deck))
                               (system-msg state :runner
                                           (str "shuffles " (join ", " (map :title targets))
                                                " into their stack"))
                               (shuffle! state :runner :deck)
                               (effect-completed state side (make-result eid cost-name)))}
                    card nil)
    cost-name))

(defn- cost-handler
  "Calls the relevant function for a cost depending on the keyword passed in"
  ([state side card action costs cost] (cost-handler state side (make-eid state) card action costs cost))
  ([state side eid card action costs cost]
   (case (first cost)
     :click (do (trigger-event state side
                               (if (= side :corp) :corp-spent-click :runner-spent-click)
                               (first (keep :action action)) (:click (into {} costs)))
                (swap! state assoc-in [side :register :spent-click] true)
                (let [r (deduce state side cost) ]
                  (effect-completed state side (make-result eid r))
                  r))
     :forfeit (pay-forfeit state side eid card (second cost))
     :hardware (pay-trash state side eid card "piece of hardware" (second cost) (every-pred installed? #(is-type? % :hardware) (complement facedown?)))
     :program (pay-trash state side eid card "program" (second cost) (every-pred installed? #(is-type? % :program) (complement facedown?)))

     ;; Connection
     :connection (pay-trash state side eid card "connection" (second cost) (every-pred installed? #(has-subtype? % "Connection") (complement facedown?)))

     ;; Rezzed ICE
     :ice (pay-trash state :corp card :ice (second cost) (every-pred rezzed? ice?) {:cause :ability-cost :keep-server-alive true})

     :tag (let [r (deduce state :runner cost)]
            (effect-completed state side (make-result eid r))
            r)
     :net-damage (damage state side eid :net (second cost) {:unpreventable true})
     :mill (let [r (mill state side (second cost))]
             (effect-completed state side (make-result eid r))
             r)

     ;; Shuffle installed runner cards into the stack (eg Degree Mill)
     :shuffle-installed-to-stack (pay-shuffle-installed-to-stack state side eid card (second cost))

     ;; Else
     (let [r (deduce state side cost)]
       (effect-completed state side (make-result eid r))
       r))))

(defn pay
  "Deducts each cost from the player.
  args format as follows with each being optional ([:click 1 :credit 0] [:forfeit] {:action :corp-click-credit})
  The map with :action was added for Jeeves so we can log what each click was used on"
  [state side card & args]
  (let [raw-costs (not-empty (remove map? args))
        action (not-empty (filter map? args))]
    (when-let [costs (apply can-pay? state side (:title card) raw-costs)]
        (->> costs
             (map (partial cost-handler state side (make-eid state) card action costs))
             (filter some?)
             (interpose " and ")
             (apply str)))))

(defn- pay-sync-next
  [state side eid costs card action msgs]
  (if (empty? costs)
    (effect-completed state side (make-result eid msgs))
    (when-completed (cost-handler state side card action costs (first costs))
                    (pay-sync-next state side eid (next costs) card action (conj msgs async-result)))))

(defn pay-sync
  "Same as pay, but awaitable with when-completed. "
  [state side eid card & args]
  (let [raw-costs (not-empty (remove map? args))
        action (not-empty (filter map? args))]
    (if-let [costs (apply can-pay? state side (:title card) raw-costs)]
      (when-completed (pay-sync-next state side costs card action [])
                      (effect-completed state side
                                        (make-result eid (->> async-result
                                                              (filter some?)
                                                              (interpose " and ")
                                                              (apply str)))))
      (effect-completed state side (make-result eid nil)))))

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ (or % 0) (last r)))))

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (trigger-event state side (if (= side :corp) :corp-loss :runner-loss) r)
    (if (= (last r) :all)
      (swap! state assoc-in [side (first r)] 0)
      (deduce state side r))))

(defn play-cost-bonus [state side costs]
  (swap! state update-in [:bonus :play-cost] #(merge-costs (concat % costs))))

(defn play-cost [state side card all-cost]
  (vec (map #(if (keyword? %) % (max % 0))
            (-> (concat all-cost (get-in @state [:bonus :play-cost])
                        (when-let [playfun (:play-cost-bonus (card-def card))]
                          (playfun state side (make-eid state) card nil)))
                merge-costs flatten))))

(defn rez-cost-bonus [state side n]
  (swap! state update-in [:bonus :cost] (fnil #(+ % n) 0)))

(defn rez-cost [state side {:keys [cost] :as card}]
  (when-not (nil? cost)
    (-> (if-let [rezfun (:rez-cost-bonus (card-def card))]
          (+ cost (rezfun state side (make-eid state) card nil))
          cost)
        (+ (or (get-in @state [:bonus :cost]) 0))
        (max 0))))

(defn run-cost-bonus [state side n]
  (swap! state update-in [:bonus :run-cost] #(merge-costs (concat % n))))

(defn click-run-cost-bonus [state side & n]
  (swap! state update-in [:bonus :click-run-cost] #(merge-costs (concat % n))))

(defn trash-cost-bonus [state side n]
  (swap! state update-in [:bonus :trash] (fnil #(+ % n) 0)))

(defn trash-cost [state side {:keys [trash] :as card}]
  (when-not (nil? trash)
    (-> (if-let [trashfun (:trash-cost-bonus (card-def card))]
          (+ trash (trashfun state side (make-eid state) card nil))
          trash)
        (+ (or (get-in @state [:bonus :trash]) 0))
        (max 0))))

(defn install-cost-bonus [state side n]
  (swap! state update-in [:bonus :install-cost] #(merge-costs (concat % n))))

(defn ignore-install-cost [state side b]
  (swap! state assoc-in [:bonus :ignore-install-cost] b))

(defn ignore-install-cost? [state side]
  (get-in @state [:bonus :ignore-install-cost]))

(defn clear-install-cost-bonus [state side]
  (swap! state update-in [:bonus] dissoc :install-cost)
  (swap! state update-in [:bonus] dissoc :ignore-install-cost))

(defn install-cost [state side card all-cost]
  (vec (map #(if (keyword? %) % (max % 0))
            (-> (concat (get-in @state [:bonus :install-cost]) all-cost
                        (when-let [instfun (:install-cost-bonus (card-def card))]
                          (instfun state side (make-eid state) card nil)))
                merge-costs flatten))))

(defn modified-install-cost
  "Returns the number of credits required to install the given card, after modification effects including
  the argument 'additional', which is a vector of costs like [:credit -1]. Allows cards like
  Street Peddler to pre-calculate install costs without actually triggering the install."
  ([state side card] (modified-install-cost state side card nil))
  ([state side card additional]
   (trigger-event state side :pre-install card)
   (let [cost (install-cost state side card (merge-costs (concat additional [:credit (:cost card)])))]
     (swap! state update-in [:bonus] dissoc :install-cost)
     (swap! state update-in [:bonus] dissoc :ignore-install-cost)
     cost)))
