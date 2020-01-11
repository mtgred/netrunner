(in-ns 'game.core)

(declare get-zones ice-index parse-command swap-ice swap-installed)

(defn say
  "Prints a message to the log as coming from the given username. The special user string
  __system__ shows no user name."
  [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))
        text (if (= (.trim text) "null") " null" text)]
    (if-let [command (parse-command text)]
      (when (and (not= side nil) (not= side :spectator))
        (command state side)
        (swap! state update-in [:log] #(conj % {:user nil :text (str "[!]" (:username author) " uses a command: " text)})))
      (swap! state update-in [:log] #(conj % {:user author :text text})))
    (swap! state assoc :typing (remove #{(:username author)} (:typing @state)))))

(defn typing
  "Updates game state list with username of whoever is typing"
  [state side {:keys [user]}]
  (let [author (:username (or user (get-in @state [side :user])))]
    (swap! state assoc :typing (distinct (conj (:typing @state) author)))
    ;; say something to force update in client side rendering
    (say state side {:user "__system__" :text "typing"})))

(defn typingstop
  "Clears typing flag from game state for user"
  [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))]
    (swap! state assoc :typing (remove #{(:username author)} (:typing @state)))
    ;; say something to force update in client side rendering
    (say state side {:user "__system__" :text "typing"})))

(defn system-say
  "Prints a system message to log (`say` from user __system__)"
  ([state side text] (system-say state side text nil))
  ([state side text {:keys [hr]}]
   (say state side {:user "__system__" :text (str text (when hr "[hr]"))})))

(defn system-msg
  "Prints a message to the log without a username."
  ([state side text] (system-msg state side text nil))
  ([state side text args]
   (let [username (get-in @state [side :user :username])]
     (system-say state side (str username " " text ".") args))))

(defn enforce-msg
  "Prints a message related to a rules enforcement on a given card.
  Example: 'Architect cannot be trashed while installed.'"
  [state card text]
  (say state nil {:user (get-in card [:title]) :text (str (:title card) " " text ".")}))

(defn indicate-action
  [state side args]
  (system-say state side
              (str "[!] Please pause, " (if (= side :corp) "Corp" "Runner") " is acting."))
  (toast state side
         "You have indicated action to your opponent"
         "info"
         {:time-out 2000 :close-button false})
  (toast state (if (= side :corp) :runner :corp)
         "Pause please, opponent is acting"
         "info"
         {:time-out 5000 :close-button true}))

(defn play-sfx
  "Adds a sound effect to play to the sfx queue.
  Each SFX comes with a unique ID, so each client can track for themselves which sounds have already been played.
  The sfx queue has size limited to 3 to limit the sound torrent tabbed out or lagged players will experience."
  [state side sfx]
  (when-let [current-id (get-in @state [:sfx-current-id])]
    (do
      (swap! state update-in [:sfx] #(take 3 (conj % {:id (inc current-id) :name sfx})))
      (swap! state update-in [:sfx-current-id] inc))))

;;; "ToString"-like methods
(defn card-str
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state {:keys [zone host title facedown] :as card} {:keys [visible] :as args}]
  (str (if (corp? card)
         ; Corp card messages
         (str (if (or (rezzed? card) visible) title (if (ice? card) "ICE" "a card"))
              ; Hosted cards do not need "in server 1" messages, host has them
              (if-not host
                (str (cond (ice? card)
                           " protecting "

                           (is-root? zone)
                           " in the root of "

                           :else " in ")
                     ;TODO add naming of scoring area of corp/runner
                     (zone->name (or (second zone) zone)) ;; handles [:hand] as well as [:servers :hq]
                     (if (ice? card) (str " at position " (ice-index state card))))))
         ; Runner card messages
         (if (or facedown visible) "a facedown card" title))
       (if host (str " hosted on " (card-str state host))))))

(defn name-zone
  "Gets a string representation for the given zone."
  [side zone]
  (match (vec zone)
         [:hand] (if (= side "Runner") "Grip" "HQ")
         [:discard] (if (= side "Runner") "Heap" "Archives")
         [:deck] (if (= side "Runner") "Stack" "R&D")
         [:rig _] "Rig"
         [:servers :hq _] "the root of HQ"
         [:servers :rd _] "the root of R&D"
         [:servers :archives _] "the root of Archives"
         :else (zone->name (second zone))))


;;; In-game chat commands
(defn set-adv-counter [state side target value]
  (set-prop state side target :advance-counter value)
  (system-msg state side (str "sets advancement counters to " value " on "
                              (card-str state target)))
  (trigger-event state side :advancement-placed target))

(defn command-adv-counter [state side value]
  (resolve-ability state side
                   {:effect (effect (set-adv-counter target value))
                    :choices {:card (fn [t] (same-side? (:side t) side))}}
                   (map->Card {:title "/adv-counter command"}) nil))

(defn command-counter-smart [state side args]
  (resolve-ability
    state side
    {:effect (req (let [existing (:counter target)
                        value (if-let [n (string->num (first args))] n 0)
                        counter-type (cond (= 1 (count existing)) (first (keys existing))
                                     (can-be-advanced? target) :advance-counter
                                     (and (agenda? target) (is-scored? state side target)) :agenda
                                     (and (runner? target) (has-subtype? target "Virus")) :virus)
                        advance (= :advance-counter counter-type)]
                    (cond
                      advance
                      (set-adv-counter state side target value)

                      (not counter-type)
                      (toast state side
                             (str "Could not infer what counter type you mean. Please specify one manually, by typing "
                                  "'/counter TYPE " value "', where TYPE is advance, agenda, credit, power, or virus.")
                             "error" {:time-out 0 :close-button true})

                      :else
                      (do (set-prop state side target :counter (merge (:counter target) {counter-type value}))
                          (system-msg state side (str "sets " (name counter-type) " counters to " value " on "
                                                      (card-str state target)))))))
     :choices {:card (fn [t] (same-side? (:side t) side))}}
    (map->Card {:title "/counter command"}) nil))

(defn command-facedown [state side]
  (resolve-ability state side
                   {:prompt "Select a card to install facedown"
                    :choices {:card #(and (runner? %)
                                          (in-hand? %))}
                    :effect (effect (runner-install target {:facedown true}))}
                   (map->Card {:title "/faceup command"}) nil))

(defn command-counter [state side args]
  (cond
    (empty? args)
    (command-counter-smart state side `("1"))

    (= 1 (count args))
    (command-counter-smart state side args)

    :else
    (let [typestr (.toLowerCase (first args))
          value (if-let [n (string->num (second args))] n 1)
          one-letter (if (<= 1 (.length typestr)) (.substring typestr 0 1) "")
          two-letter (if (<= 2 (.length typestr)) (.substring typestr 0 2) one-letter)
          counter-type (cond (= "v" one-letter) :virus
                             (= "p" one-letter) :power
                             (= "c" one-letter) :credit
                             (= "ag" two-letter) :agenda
                             :else :advance-counter)
          advance (= :advance-counter counter-type)]
      (if advance
        (command-adv-counter state side value)
        (resolve-ability state side
                       {:effect (effect (set-prop target :counter (merge (:counter target) {counter-type value}))
                                        (system-msg (str "sets " (name counter-type) " counters to " value " on "
                                                         (card-str state target))))
                        :choices {:card (fn [t] (same-side? (:side t) side))}}
                       (map->Card {:title "/counter command"}) nil)))))

(defn command-rezall [state side value]
  (resolve-ability state side
    {:optional {:prompt "Rez all cards and turn cards in archives faceup?"
                :yes-ability {:effect (req
                                        (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                                        (doseq [c (all-installed state side)]
                                          (when-not (rezzed? c)
                                            (rez state side c {:ignore-cost :all-costs :force true}))))}}}
    (map->Card {:title "/rez-all command"}) nil))

(defn command-roll [state side value]
  (system-msg state side (str "rolls a " value " sided die and rolls a " (inc (rand-int value)))))

(defn command-undo-click
  "Resets the game state back to start of the click"
  [state side]
  (when-let [click-state (:click-state @state)]
    (when (= (:active-player @state) side)
      (reset! state (assoc click-state :log (:log @state) :click-state click-state :run nil))
      (doseq [s [:runner :corp]]
        (toast state s "Game reset to start of click")))))

(defn command-undo-turn
  "Resets the entire game state to how it was at end-of-turn if both players agree"
  [state side]
  (when-let [turn-state (:turn-state @state)]
    (swap! state assoc-in [side :undo-turn] true)
    (when (and (-> @state :runner :undo-turn) (-> @state :corp :undo-turn))
      (reset! state (assoc turn-state :log (:log @state) :turn-state turn-state))
      (doseq [s [:runner :corp]]
        (toast state s "Game reset to start of turn")))))

(defn command-close-prompt [state side]
  (when-let [fprompt (-> @state side :prompt first)]
    (swap! state update-in [side :prompt] rest)
    (swap! state dissoc-in [side :selected])
    (effect-completed state side (:eid fprompt))))

(defn command-install-ice
  [state side]
  (when (= side :corp)
    (resolve-ability
      state side
      {:prompt "Select a piece of ice to install"
       :choices {:card #(and (ice? %)
                             (#{[:hand]} (:zone %)))}
       :effect (effect
                 (continue-ability
                   (let [chosen-ice target]
                     {:prompt "Choose a server"
                      :choices (req servers)
                      :effect (effect
                                (continue-ability
                                  (let [chosen-server target
                                        num-ice (count (get-in (:corp @state)
                                                               (conj (server->zone state target) :ices)))]
                                    {:prompt "Which position to install in? (0 is innermost)"
                                     :choices (vec (reverse (map str (range (inc num-ice)))))
                                     :effect (effect (corp-install chosen-ice chosen-server
                                                                   {:no-install-cost true
                                                                    :index (str->int target)}))})
                                  card nil))})
                   card nil))}
      (map->Card {:title "/install-ice command"}) nil)))

(defn command-peek
  [state side n]
  (show-prompt
    state side
    nil
    (str "The top " (quantify n "card")
         " of your deck " (if (< 1 n) "are" "is") " (top first): "
         (->> (get-in @state [side :deck])
              (take n)
              (map :title)
              (string/join ", ")))
    ["Done"]
    identity
    {:priority 10}))

(defn command-summon
  [state side args]
  (let [s-card (server-card (string/join " " args))
        card (when (and s-card (same-side? (:side s-card) side))
               (build-card s-card))]
    (when card
      (swap! state update-in [side :hand] #(concat % (zone :hand [card]))))))

(defn command-host
  [state side]
  (let [f (if (= :corp side) corp? runner?)]
    (resolve-ability
      state side
      {:prompt "Select the card to be hosted"
       :choices {:card #(and (f %)
                             (installed? %))}
       :async true
       :effect (effect
                 (continue-ability
                   (let [h1 target]
                     {:prompt "Select the card to host the first card"
                      :choices {:card #(and (f %)
                                            (installed? %)
                                            (not (same-card? % h1)))}
                      :effect (effect (host target h1 nil))})
                   nil nil))}
      nil nil)))

(defn command-trash
  [state side]
  (let [f (if (= :corp side) corp? runner?)]
    (resolve-ability
      state side
      {:prompt "Select a card to trash"
       :choices {:card #(f %)}
       :effect (effect (trash eid target {:unpreventable true}))}
      nil nil)))

(defn parse-command [text]
  (let [[command & args] (split text #" ")
        value (if-let [n (string->num (first args))] n 1)
        num   (if-let [n (-> args first (safe-split #"#") second string->num)] (dec n) 0)]
    (when (<= (count args) 2)
      (if (= (ffirst args) \#)
        (case command
          "/deck"       #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :deck {:front true})
          "/discard"    #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :discard)
          nil)
        (case command
          "/adv-counter" #(command-adv-counter %1 %2 value)
          "/bp"         #(swap! %1 assoc-in [%2 :bad-publicity :base] (max 0 value))
          "/card-info"  #(resolve-ability %1 %2
                                          {:effect (effect (system-msg (str "shows card-info of "
                                                                            (card-str state target)
                                                                            ": " (get-card state target))))
                                           :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                          (map->Card {:title "/card-info command"}) nil)
          "/clear-win"  clear-win
          "/click"      #(swap! %1 assoc-in [%2 :click] (max 0 value))
          "/close-prompt" command-close-prompt
          "/counter"    #(command-counter %1 %2 args)
          "/credit"     #(swap! %1 assoc-in [%2 :credit] (max 0 value))
          "/deck"       #(toast %1 %2 "/deck number takes the format #n")
          "/discard"    #(toast %1 %2 "/discard number takes the format #n")
          "/discard-random" #(move %1 %2 (rand-nth (get-in @%1 [%2 :hand])) :discard)
          "/draw"       #(draw %1 %2 (max 0 value))
          "/end-run"    #(when (= %2 :corp) (end-run %1 %2))
          "/error"      show-error-toast
          "/facedown"   #(when (= %2 :runner) (command-facedown %1 %2))
          "/handsize"   #(swap! %1 assoc-in [%2 :hand-size :mod] (- value (get-in @%1 [%2 :hand-size :base])))
          "/host"       command-host
          "/install-ice" command-install-ice
          "/jack-out"   #(when (= %2 :runner) (jack-out %1 %2 nil))
          "/link"       #(swap! %1 assoc-in [%2 :link] (max 0 value))
          "/memory"     #(swap! %1 assoc-in [%2 :memory :used] (- (+ (get-in @%1 [:runner :memory :base])
                                                                     (get-in @%1 [:runner :memory :mod]))
                                                                  value))
          "/move-bottom"  #(resolve-ability %1 %2
                                            {:prompt "Select a card in hand to put on the bottom of your deck"
                                             :effect (effect (move target :deck))
                                             :choices {:card (fn [t] (and (same-side? (:side t) %2)
                                                                          (in-hand? t)))}}
                                            (map->Card {:title "/move-bottom command"}) nil)
          "/move-deck"   #(resolve-ability %1 %2
                                           {:prompt "Select a card to move to the top of your deck"
                                            :effect (req (let [c (deactivate %1 %2 target)]
                                                           (move %1 %2 c :deck {:front true})))
                                            :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                           (map->Card {:title "/move-deck command"}) nil)
          "/move-hand"  #(resolve-ability %1 %2
                                          {:prompt "Select a card to move to your hand"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :hand)))
                                           :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                          (map->Card {:title "/move-hand command"}) nil)
          "/peek"       #(command-peek %1 %2 value)
          "/psi"        #(when (= %2 :corp) (psi-game %1 %2
                                                      (map->Card {:title "/psi command" :side %2})
                                                      {:equal  {:msg "resolve equal bets effect"}
                                                       :not-equal {:msg "resolve unequal bets effect"}}))
          "/rez"        #(when (= %2 :corp)
                           (resolve-ability %1 %2
                                            {:effect (effect (rez target {:ignore-cost :all-costs :force true}))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (map->Card {:title "/rez command"}) nil))
          "/rez-all"    #(when (= %2 :corp) (command-rezall %1 %2 value))
          "/rfg"        #(resolve-ability %1 %2
                                          {:prompt "Select a card to remove from the game"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :rfg)))
                                           :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                          (map->Card {:title "/rfg command"}) nil)
          "/roll"       #(command-roll %1 %2 value)
          "/summon"     #(command-summon %1 %2 args)
          "/swap-ice"   #(when (= %2 :corp)
                           (resolve-ability
                             %1 %2
                             {:prompt "Select two installed ice to swap"
                              :choices {:max 2
                                        :all true
                                        :card (fn [c] (and (installed? c)
                                                           (ice? c)))}
                              :effect (effect (swap-ice (first targets) (second targets)))}
                             (map->Card {:title "/swap-ice command"}) nil))
          "/swap-installed" #(when (= %2 :corp)
                               (resolve-ability
                                 %1 %2
                                 {:prompt "Select two installed non-ice to swap"
                                  :choices {:max 2
                                            :all true
                                            :card (fn [c] (and (installed? c)
                                                               (corp? c)
                                                               (not (ice? c))))}
                                  :effect (effect (swap-installed (first targets) (second targets)))}
                                 (map->Card {:title "/swap-installed command"}) nil))
          "/tag"        #(swap! %1 assoc-in [%2 :tag :base] (max 0 value))
          "/take-brain" #(when (= %2 :runner) (damage %1 %2 :brain (max 0 value)))
          "/take-meat"  #(when (= %2 :runner) (damage %1 %2 :meat  (max 0 value)))
          "/take-net"   #(when (= %2 :runner) (damage %1 %2 :net   (max 0 value)))
          "/trace"      #(when (= %2 :corp) (init-trace %1 %2
                                                        (map->Card {:title "/trace command" :side %2})
                                                        {:base (max 0 value)
                                                         :msg "resolve successful trace effect"}))
          "/trash"      command-trash
          "/undo-click" #(command-undo-click %1 %2)
          "/undo-turn"  #(command-undo-turn %1 %2)
          nil)))))

(defn corp-install-msg
  "Gets a message describing where a card has been installed from. Example: Interns."
  [card]
  (str "install " (if (:seen card) (:title card) "an unseen card") " from " (name-zone :corp (:zone card))))

(defn turn-message
  "Prints a message for the start or end of a turn, summarizing credits and cards in hand."
  [state side start-of-turn]
  (let [pre (if start-of-turn "started" "is ending")
        hand (if (= side :runner) "their Grip" "HQ")
        cards (count (get-in @state [side :hand]))
        credits (get-in @state [side :credit])
        text (str pre " their turn " (:turn @state) " with " credits " [Credit] and " cards " cards in " hand)]
    (system-msg state side text {:hr (not start-of-turn)})))

(defn event-title
  "Gets a string describing the internal engine event keyword"
  [event]
  (if (keyword? event)
    (name event)
    (str event)))
