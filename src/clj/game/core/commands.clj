(ns game.core.commands
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as string]
   [game.core.actions :refer [score]]
   [game.core.board :refer [all-installed server->zone]]
   [game.core.card :refer [agenda? can-be-advanced? corp? get-card
                           has-subtype? ice? in-hand? installed? rezzed? runner?]]
   [game.core.change-vals :refer [change]]
   [game.core.charge :refer [charge-card]]
   [game.core.damage :refer [damage]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [resolve-ability trigger-event]]
   [game.core.flags :refer [is-scored?]]
   [game.core.hosting :refer [host]]
   [game.core.identities :refer [disable-identity disable-card enable-card]]
   [game.core.initializing :refer [card-init deactivate make-card]]
   [game.core.installing :refer [corp-install runner-install]]
   [game.core.mark :refer [identify-mark set-mark]]
   [game.core.moving :refer [move swap-ice swap-installed trash]]
   [game.core.prompt-state :refer [remove-from-prompt-queue]]
   [game.core.prompts :refer [show-prompt]]
   [game.core.props :refer [set-prop]]
   [game.core.psi :refer [psi-game]]
   [game.core.rezzing :refer [rez derez]]
   [game.core.runs :refer [end-run get-current-encounter jack-out]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg system-say unsafe-say]]
   [game.core.servers :refer [is-central? unknown->kw]]
   [game.core.set-up :refer [build-card]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [show-error-toast toast]]
   [game.core.trace :refer [init-trace]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [clear-win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer [dissoc-in enumerate-str quantify safe-split
                       same-card? same-side? server-card string->num]]
   [jinteki.utils :refer [other-side str->int]]))

(defmulti lobby-command :command)

(defn- constrain-value
  "Constrain value to [min-value max-value]"
  [value min-value max-value]
  (min max-value (max min-value value)))

(defn- set-adv-counter [state side target value]
  (set-prop state side target :advance-counter value)
  (system-msg state side (str "sets advancement counters to " value " on "
                              (card-str state target)))
  (trigger-event state side :advancement-placed {:card target}))

(defn command-adv-counter [state side value]
  (let [value (constrain-value value 0 1000)]
    (resolve-ability state side
                     {:effect (effect (set-adv-counter target value))
                      :choices {:card (fn [t] (same-side? (:side t) side))}}
                     (make-card {:title "/adv-counter command"}) nil)))

(defn command-save-replay [state _]
  (swap! state assoc-in [:options :save-replay] true))

(defn command-bug-report [state side]
  (swap! state update :bug-reported (fnil inc -1))
  (let [title "[EDITME] Please give a short description of your bug here"
        body (str "Link to bug replay: https://jinteki.net/bug-report/" (:gameid @state)
                  "?b=" (:bug-reported @state) "\n\n"
                  "Description:\n\n"
                  "[EDITME] Please describe the steps to reproduce your bug and the resulting effect here.")]
    (unsafe-say state [:div.bugreport [:div.smallwarning "!"]
                       "Thanks for helping us make the game better! The replay was saved. "
                       "Please report a bug following "
                       [:a {:target "_blank"
                            :href (str "https://github.com/mtgred/netrunner/issues/new?title="
                                       (string/replace title #" " "%20")
                                       "&body="
                                       (string/replace (string/replace body #" " "%20") #"\n" "%0A"))}
                        "this link"]
                       " to GitHub."])))

(defn command-counter-smart [state side args]
  (resolve-ability
    state side
    {:choices {:card (fn [t] (same-side? (:side t) side))}
     :effect (req (let [existing (:counter target)
                        value (constrain-value (if-let [n (string->num (first args))] n 0) 0 1000)
                        counter-type (cond (= 1 (count existing)) (first (keys existing))
                                     (can-be-advanced? state target) :advance-counter
                                     (and (agenda? target) (is-scored? state side target)) :agenda
                                     (and (runner? target) (has-subtype? target "Virus")) :virus)
                        advance (= :advance-counter counter-type)]
                    (when (not (neg? value))
                      (cond
                        advance
                        (set-adv-counter state side target value)

                        (not counter-type)
                        (toast state side
                               (str "Could not infer what counter type you mean. Please specify one manually, by typing "
                                    "'/counter TYPE " value "', where TYPE is advance, agenda, credit, power, bad publicity, or virus.")
                               "error" {:time-out 0 :close-button true})

                        :else
                        (do (update! state side (assoc-in target [:counter counter-type] value))
                            (system-msg state side (str "sets " (name counter-type) " counters to " value " on "
                                                        (card-str state target))))))))}
    (make-card {:title "/counter command"}) nil))

(defn command-enable-api-access [state _]
  (swap! state assoc-in [:options :api-access] true))

(defn command-facedown [state side]
  (resolve-ability state side
                   {:prompt "Choose a card to install facedown"
                    :waiting-prompt true
                    :choices {:card #(and (runner? %)
                                          (in-hand? %))}
                    :async true
                    :effect (effect (runner-install (make-eid state eid) target {:facedown true}))}
                   (make-card {:title "/faceup command"}) nil))

(defn command-counter [state side args]
  (cond
    (empty? args)
    (command-counter-smart state side `("1"))

    (= 1 (count args))
    (command-counter-smart state side args)

    :else
    (let [typestr (.toLowerCase (first args))
          value (constrain-value (if-let [n (string->num (second args))] n 1) 0 1000)
          one-letter (if (<= 1 (.length typestr)) (.substring typestr 0 1) "")
          two-letter (if (<= 2 (.length typestr)) (.substring typestr 0 2) one-letter)
          counter-type (cond (= "v" one-letter) :virus
                             (= "b" one-letter) :bad-publicity
                             (= "p" one-letter) :power
                             (= "c" one-letter) :credit
                             (= "ag" two-letter) :agenda
                             :else :advance-counter)
          advance (= :advance-counter counter-type)]
      (if advance
        (command-adv-counter state side value)
        (resolve-ability state side
                         {:effect (effect (update! (assoc-in target [:counter counter-type] value))
                                          (system-msg (str "sets " (name counter-type) " counters to " value " on "
                                                           (card-str state target))))
                          :choices {:card (fn [t] (same-side? (:side t) side))}}
                         (make-card {:title "/counter command"}) nil)))))

(defn rez-all
  [state side eid cards]
  (if-let [c (first cards)]
    (wait-for (rez state side c {:ignore-cost :all-costs :force true :silent (next cards)})
              (rez-all state side eid (next cards)))
    (effect-completed state side eid)))

(defn rez-all-turn-agendas-faceup
  [cards]
  (let [agendas (filter (every-pred agenda? (complement :seen)) cards)]
    (when (seq agendas)
      {:optional
       {:prompt "Turn all agendas faceup?"
        :yes-ability {:effect (req (doseq [c agendas]
                                     (update! state side (assoc c :seen true))))
                      :msg (msg "turns all agendas faceup")}}})))

(defn command-rezall
  [state side]
  (resolve-ability
    state side
    {:optional
     {:prompt "Rez all cards and turn cards in archives faceup?"
      :waiting-prompt true
      :yes-ability {:async true
                    :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                                 (wait-for
                                   (rez-all state side (remove rezzed? (all-installed state side)))
                                   (continue-ability
                                     state side
                                     (rez-all-turn-agendas-faceup (all-installed state side))
                                     card nil)))}}}
    (make-card {:title "/rez-all command"}) nil))

(defn command-roll [state side value]
  (let [value (constrain-value value 1 1000)]
    (system-msg state side (str "rolls a " value " sided die and rolls a " (inc (rand-int value))))))

(defn command-set-mark
  "Sets a central server as the mark for the turn"
  [state side [server & _]]
  (when (and (= :runner side)
             (is-central? (unknown->kw server)))
    (system-msg state side (str "sets " server " as the mark for this turn"))
    (set-mark state (unknown->kw server))))

(defn command-undo-click
  "Resets the game state back to start of the click"
  [state side]
  (when-let [last-click-state (peek (:click-states @state))]
    (when (= (:active-player @state) side)
      (let [current-log (:log @state)
            current-history (:history @state)
            previous-click-states (pop (:click-states @state))
            turn-state (:turn-state @state)
            last-click-state (assoc last-click-state
                               :log current-log
                               :click-states previous-click-states
                               :turn-state turn-state
                               :history current-history
                               :run nil)]
        (reset! state last-click-state))
      (system-say state side (str "[!] " (if (= side :corp) "Corp" "Runner") " uses the undo-click command"))
      (doseq [s [:runner :corp]]
        (toast state s "Game reset to start of click")))))

(defn command-undo-turn
  "Resets the entire game state to how it was at end-of-turn if both players agree"
  [state side]
  (when-let [turn-state (:turn-state @state)]
    (swap! state assoc-in [side :undo-turn] true)
    (when (and (-> @state :runner :undo-turn) (-> @state :corp :undo-turn))
      (let [current-log (:log @state)
            current-history (:history @state)
            original-turn-state (assoc turn-state
                                  :log current-log
                                  :history current-history
                                  :turn-state turn-state)]
        (reset! state original-turn-state))
      (doseq [s [:runner :corp]]
        (swap! state dissoc-in [s :turn-started])
        (toast state s "Game reset to start of turn")))))

(defn command-unique
  "Toggles :uniqueness of the selected card"
  [state side]
  (resolve-ability state side
                   {:effect (effect (set-prop target :uniqueness (not (:uniqueness target))))
                    :msg (msg "make " (card-str state target)
                              (when (:uniqueness target) " not") ;it was unique before
                              " unique")
                    :choices {:card (fn [t] (same-side? (:side t) side))}}
                   (make-card {:title "/unique command" :side side}) nil))

(defn command-close-prompt [state side]
  (when-let [prompt (-> @state side :prompt first)]
    (remove-from-prompt-queue state side prompt)
    (swap! state dissoc-in [side :selected])
    (effect-completed state side (:eid prompt))))

(defn command-install
  ([state side] (command-install state side nil))
  ([state side {:keys [ignore-all-cost] :as args}]
   (resolve-ability
     state side
     (if (= side :corp)
       {:prompt (str "Choose a card to install" (when ignore-all-cost " (ignoring all costs)"))
        :waiting-prompt true
        :choices {:card #(and (corp? %)
                              (not (installed? %)))}
        :async true
        :effect (req (corp-install state side eid target nil {:ignore-all-cost ignore-all-cost}))}
       {:prompt (str "Choose a card to install" (when ignore-all-cost " (ignoring all costs)"))
        :waiting-prompt true
        :choices {:card #(and (runner? %)
                              (not (installed? %)))}
        :async true
        :effect (req (runner-install state side eid target {:ignore-all-cost ignore-all-cost}))})
     (make-card {:title (str "/install" (when ignore-all-cost "-free") " command")}) nil)))

(defn command-install-free
  [state side]
  (command-install state side {:ignore-all-cost true}))

(defn command-install-ice
  [state side]
  (when (= side :corp)
    (resolve-ability
      state side
      {:prompt "Choose a piece of ice to install"
       :waiting-prompt true
       :choices {:card #(and (ice? %)
                             (#{[:hand]} (:zone %)))}
       :async true
       :effect (effect
                 (continue-ability
                   (let [chosen-ice target]
                     {:prompt "Choose a server"
                      :choices (req servers)
                      :async true
                      :effect (effect
                                (continue-ability
                                  (let [chosen-server target
                                        num-ice (count (get-in (:corp @state)
                                                               (conj (server->zone state target) :ices)))]
                                    {:prompt "Which position to install in? (0 is innermost)"
                                     :choices (vec (reverse (map str (range (inc num-ice)))))
                                     :async true
                                     :effect (effect (corp-install
                                                       (make-eid state eid)
                                                       chosen-ice chosen-server
                                                       {:no-install-cost true
                                                        :index (str->int target)}))})
                                  card nil))})
                   card nil))}
      (make-card {:title "/install-ice command"}) nil)))

(defn command-peek
  [state side n]
  (show-prompt
    state side
    nil
    (str "The top " (quantify n "card")
         " of your deck " (if (< 1 n) "are" "is") " (top->bottom): "
         (->> (get-in @state [side :deck])
              (take n)
              (map :title)
              (enumerate-str)))
    ["Done"]
    identity))

(defn command-score
  [state side]
  (when (= :corp side)
    (resolve-ability
     state side
     {:prompt "Choose an agenda to score"
      :waiting-prompt true
      :choices {:req (req (and (agenda? target)
                               (or (installed? target)
                                   (in-hand? target))))}
      :msg (msg "score " (card-str state target {:visible true}) ", ignoring all restrictions")
      :async true
      :effect (effect (score eid target {:no-req true :ignore-turn true}))}
     (make-card {:title "the '/score' command"}) nil)))

(defn command-summon
  [state side args]
  (let [card-name (string/join " " args)]
    (try
      (let [s-card (server-card card-name)
            card (when (and s-card (same-side? (:side s-card) side))
                   (build-card s-card))]
        (when card
          (swap! state update-in [side :hand] #(concat % [(assoc card :zone [:hand])]))))
      (catch Exception ex
        (toast state side (str card-name " isn't a real card"))))))

(defn command-reload-id
  [state side]
  (let [card-name (:title (get-in @state [side :identity]))]
    (try
      (let [s-card (server-card card-name)
            card (when (and s-card (same-side? (:side s-card) side))
                   (build-card s-card))]
        (if card
          (let [new-id (-> card :title server-card make-card (assoc :zone [:identity] :type "Identity"))]
            (disable-identity state side)
            (swap! state assoc-in [side :identity] new-id)
            (card-init state side new-id {:resolve-effect true :init-data true}))
          (toast state side (str card-name " isn't a valid card"))))
      (catch Exception ex
        (toast state side (str card-name " isn't a real card"))))))

(defn command-replace-id
  [state side args]
  (let [card-name (string/join " " args)]
    (try
      (let [s-card (server-card card-name)
            card (when (and s-card (same-side? (:side s-card) side))
                   (build-card s-card))]
        (if card
          (let [new-id (-> card :title server-card make-card (assoc :zone [:identity] :type "Identity"))]
            (disable-identity state side)
            (swap! state assoc-in [side :identity] new-id)
            (card-init state side new-id {:resolve-effect true :init-data true}))
          (toast state side (str card-name " isn't a valid card"))))
      (catch Exception ex
        (toast state side (str card-name " isn't a real card"))))))

(defn command-host
  [state side]
  (let [f (if (= :corp side) corp? runner?)]
    (resolve-ability
      state side
      {:prompt "Choose the card to be hosted"
       :waiting-prompt true
       :choices {:card #(and (f %)
                             (installed? %))}
       :async true
       :effect (effect
                 (continue-ability
                   (let [h1 target]
                     {:prompt "Choose the card to host the first card"
                      :choices {:card #(and (f %)
                                            (installed? %)
                                            (not (same-card? % h1)))}
                      :effect (effect (host target h1 nil))})
                   nil nil))}
      nil nil)))

(defn command-derez
  [state side]
  (when (= :corp side)
    (resolve-ability
     state side
     {:prompt "Choose a card to derez"
      :waiting-prompt true
      :choices {:card #(rezzed? %)}
      :async true
      :effect (effect (derez eid target {:no-event true}))}
     nil nil)))

(defn command-trash
  [state side]
  (let [f (if (= :corp side) corp? runner?)]
    (resolve-ability
      state side
      {:prompt "Choose a card to trash"
       :waiting-prompt true
       :choices {:card #(f %)}
       :async true
       :effect (effect (trash eid target {:unpreventable true}))}
      nil nil)))

(defn command-swap-sides
  [state side]
  (swap! state dissoc-in [side :command-info :ignore-swap-sides])
  (if (get-in @state [(other-side side) :command-info :ignore-swap-sides])
    (toast state side "your opponent has indicated that they do not wish to swap sides")
    (resolve-ability
      state (other-side side)
      {:prompt "Your opponent wishes to swap sides"
       :waiting-prompt true
       :choices ["Accept" "Decline" "Don't ask me again"]
       :effect (req (cond
                      (= target "Decline")
                      (toast state (other-side side) "your opponent does not wish to swap sides at this time")
                      (= target "Don't ask me again")
                      (do (toast state (other-side side) "your opponent does not wish to swap sides")
                          (swap! state assoc-in [side :command-info :ignore-swap-sides] true))
                      (= target "Accept")
                      (do (system-msg state side "accepts the request to swap sides. Players swap sides")
                          (lobby-command {:command :swap-sides
                                          :gameid (:gameid @state)}))))}
      nil nil)))

(defn parse-command
  [state text]
  (let [[command & args] (safe-split text #" ")
        value (if-let [n (string->num (first args))] n 1)
        num   (if-let [n (-> args first (safe-split #"#") second string->num)] (dec n) 0)
        res
        (if (= (ffirst args) \#)
          (case command
            "/deck"       #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :deck {:front true})
            "/discard"    #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :discard)
            nil)
          (case command
            "/adv-counter" #(command-adv-counter %1 %2 value)
            "/bp"         #(swap! %1 assoc-in [%2 :bad-publicity :base] (constrain-value value -1000 1000))
            "/bug"        command-bug-report
            "/card-info"  #(resolve-ability %1 %2
                                            {:effect (effect (system-msg (str "shows card-info of "
                                                                              (card-str state target)
                                                                              ": " (get-card state target))))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/card-info command"}) nil)
            "/charge"     #(resolve-ability %1 %2
                                            {:prompt "Choose an installed card"
                                             :waiting-prompt true
                                             :async true
                                             :effect (req (charge-card %1 %2 eid target))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/charge command"}) nil)
            "/clear-win"  clear-win
            "/click"      #(swap! %1 assoc-in [%2 :click] (constrain-value value 0 1000))
            "/close-prompt" command-close-prompt
            "/counter"    #(command-counter %1 %2 args)
            "/credit"     #(swap! %1 assoc-in [%2 :credit] (constrain-value value 0 1000))
            "/deck"       #(toast %1 %2 "/deck number takes the format #n")
            "/derez"      command-derez
            "/disable-card" #(resolve-ability %1 %2
                                              {:prompt "Choose a card to disable"
                                               :waiting-prompt true
                                               :effect (req (disable-card state side target))
                                               :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                              (make-card {:title "/disable-card command"}) nil)
            "/discard"    #(toast %1 %2 "/discard number takes the format #n")
            "/discard-random" #(move %1 %2 (rand-nth (get-in @%1 [%2 :hand])) :discard)
            "/draw"       #(draw %1 %2 (make-eid %1) (constrain-value value 0 1000))
            "/enable-card" #(resolve-ability %1 %2
                                             {:prompt "Choose a card to enable"
                                              :waiting-prompt true
                                              :effect (req (enable-card state side target))
                                              :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                             (make-card {:title "/enable-card command"}) nil)
            "/end-run"    (fn [state side]
                            (when (and (= side :corp)
                                       (:run @state))
                              (end-run state side (make-eid state) nil)))
            "/enable-api-access" command-enable-api-access
            "/error"      show-error-toast
            "/facedown"   #(when (= %2 :runner) (command-facedown %1 %2))
            "/handsize"   #(change %1 %2 {:key :hand-size
                                          :delta (- (constrain-value value -1000 1000)
                                                    (get-in @%1 [%2 :hand-size :total]))})
            "/host"       command-host
            "/install" command-install
            "/install-ice" command-install-ice
            "/install-free" command-install-free
            "/jack-out"   (fn [state side]
                            (when (and (= side :runner)
                                       (or (:run @state)
                                           (get-current-encounter state)))
                              (jack-out state side (make-eid state))))
            "/link"       (fn [state side]
                            (when (= side :runner)
                              (swap! state assoc-in [:runner :link] (constrain-value value 0 1000))))
            "/mark"       #(when (= %2 :runner) (identify-mark %1))
            "/memory"     (fn [state side]
                            (when (= side :runner)
                              (swap! state assoc-in [:runner :memory :used] (constrain-value value -1000 1000))))
            "/move-bottom"  #(resolve-ability %1 %2
                                              {:prompt "Choose a card in hand to put on the bottom of your deck"
                                               :waiting-prompt true
                                               :effect (effect (move target :deck))
                                               :choices {:card (fn [t] (and (same-side? (:side t) %2)
                                                                            (in-hand? t)))}}
                                              (make-card {:title "/move-bottom command"}) nil)
            "/move-deck"   #(resolve-ability %1 %2
                                             {:prompt "Choose a card to move to the top of your deck"
                                              :waiting-prompt true
                                              :effect (req (let [c (deactivate %1 %2 target)]
                                                             (move %1 %2 c :deck {:front true})))
                                              :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                             (make-card {:title "/move-deck command"}) nil)
            "/move-hand"  #(resolve-ability %1 %2
                                            {:prompt "Choose a card to move to your hand"
                                             :waiting-prompt true
                                             :effect (req (let [c (deactivate %1 %2 target)]
                                                            (move %1 %2 c :hand)))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/move-hand command"}) nil)
            "/peek"       #(command-peek %1 %2 value)
            "/psi"        #(when (= %2 :corp) (psi-game %1 %2
                                                        (make-card {:title "/psi command" :side %2})
                                                        {:equal  {:msg "resolve equal bets effect"}
                                                         :not-equal {:msg "resolve unequal bets effect"}}))
            "/reload-id"  command-reload-id
            "/replace-id" #(command-replace-id %1 %2 args)
            "/rez"        #(when (= %2 :corp)
                             (resolve-ability %1 %2
                                              {:choices {:card (fn [t] (same-side? (:side t) %2))}
                                               :async true
                                               :effect (effect (rez eid target {:ignore-cost :all-costs :force true}))}
                                              (make-card {:title "/rez command"}) nil))
            "/rez-all"    #(when (= %2 :corp) (command-rezall %1 %2))
            "/rez-free"   #(when (= %2 :corp)
                             (resolve-ability %1 %2
                                              {:choices {:card (fn [t] (same-side? (:side t) %2))}
                                               :async true
                                               :effect (effect (disable-card target)
                                                               (rez eid target {:ignore-cost :all-costs :force true})
                                                               (enable-card (get-card state target)))}
                                              (make-card {:title "/rez command"}) nil))
            "/rfg"        #(resolve-ability %1 %2
                                            {:prompt "Choose a card"
                                             :waiting-prompt true
                                             :effect (req (let [c (deactivate %1 %2 target)]
                                                            (move %1 %2 c :rfg)))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/rfg command"}) nil)
            "/roll"       #(command-roll %1 %2 value)
            "/sabotage"   #(when (= %2 :runner) (resolve-ability %1 %2 (sabotage-ability (constrain-value value 0 1000)) nil nil))
            "/save-replay" command-save-replay
            "/set-mark"   #(command-set-mark %1 %2 args)
            "/score"      command-score
            "/show-hand" #(resolve-ability %1 %2
                                           {:effect (effect (system-msg (str
                                                                          (if (= :corp %2)
                                                                            "shows cards from HQ: "
                                                                            "shows cards from the grip: ")
                                                                          (enumerate-str (sort (map :title (:hand (if (= side :corp) corp runner))))))))}
                                           nil nil)
            "/summon"     #(command-summon %1 %2 args)
            "/swap-ice"   #(when (= %2 :corp)
                             (resolve-ability
                               %1 %2
                               {:prompt "Choose two installed ice to swap"
                                :waiting-prompt true
                                :choices {:max 2
                                          :all true
                                          :card (fn [c] (and (installed? c)
                                                             (ice? c)))}
                                :effect (effect (swap-ice (first targets) (second targets)))}
                               (make-card {:title "/swap-ice command"}) nil))
            "/swap-installed" #(when (= %2 :corp)
                                 (resolve-ability
                                   %1 %2
                                   {:prompt "Choose two installed non-ice to swap"
                                    :waiting-prompt true
                                    :choices {:max 2
                                              :all true
                                              :card (fn [c] (and (installed? c)
                                                                 (corp? c)
                                                                 (not (ice? c))))}
                                    :effect (effect (swap-installed (first targets) (second targets)))}
                                   (make-card {:title "/swap-installed command"}) nil))
            "/swap-sides" #(command-swap-sides %1 %2)
            "/tag"        #(swap! %1 assoc-in [%2 :tag :base] (constrain-value value 0 1000))
            "/take-core" #(when (= %2 :runner) (damage %1 %2 (make-eid %1) :brain (constrain-value value 0 1000)
                                                       {:card (make-card {:title "/damage command" :side %2})}))
            "/take-meat"  #(when (= %2 :runner) (damage %1 %2 (make-eid %1) :meat  (constrain-value value 0 1000)
                                                        {:card (make-card {:title "/damage command" :side %2})}))
            "/take-net"   #(when (= %2 :runner) (damage %1 %2 (make-eid %1) :net   (constrain-value value 0 1000)
                                                        {:card (make-card {:title "/damage command" :side %2})}))
            "/trace"      #(when (= %2 :corp) (init-trace %1 %2
                                                          (make-card {:title "/trace command" :side %2})
                                                          {:base (constrain-value value -1000 1000)
                                                           :msg "resolve successful trace effect"}))
            "/trash"      command-trash
            "/undo-click" #(command-undo-click %1 %2)
            "/undo-turn"  #(command-undo-turn %1 %2)
            "/unique"     #(command-unique %1 %2)
            nil))]
    (when res
      (swap! state update-in [:command-log] (fnil #(concat % [{:command command
                                                               :timestamp (inst/now)}])
                                                  [])))
    res))
