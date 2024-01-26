(ns game.core.trace
  (:require
    [game.core.costs :refer [total-available-credits]]
    [game.core.effects :refer [any-effects sum-effects get-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [can-trigger? pay register-ability-type resolve-ability trigger-event-simult trigger-event-sync]]
    [game.core.link :refer [get-link]]
    [game.core.prompts :refer [clear-wait-prompt show-trace-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg system-say]]
    [game.macros :refer [continue-ability effect wait-for]]))

(defn- determine-initiator
  [state {:keys [player]}]
  (let [runner? (any-effects state nil :trace-runner-spends-first)]
    (cond
      runner? :runner
      (some? player) player
      :else :corp)))

(defn- corp-start?
  [trace]
  (= :corp (:player trace)))

(defn- resolve-trace
  "Compares trace strength and link strength and triggers the appropriate effects."
  [state side eid card {:keys [player other base bonus link strength] :as trace} boost]
  (let [corp-strength (if (corp-start? trace)
                        strength
                        ((fnil + 0 0 0) base bonus boost))
        runner-strength (if (corp-start? trace)
                          ((fnil + 0 0) link boost)
                          strength)
        trigger-trace (select-keys trace [:player :other :base :bonus :link :ability :strength])]
    (wait-for (pay state other (make-eid state eid) card [:credit boost])
              (let [payment-str (:msg async-result)]
                (system-msg state other (str payment-str
                                             " to increase " (if (corp-start? trace) "link" "trace")
                                             " strength to " (if (corp-start? trace)
                                                               runner-strength
                                                               corp-strength))))
              (clear-wait-prompt state player)
              (let [successful (> corp-strength runner-strength)
                    which-ability (assoc (if successful
                                           (:successful trace)
                                           (:unsuccessful trace))
                                         :eid (make-eid state))]
                (system-say state player (str "The trace was " (when-not successful "un") "successful."))
                (wait-for (trigger-event-simult state :corp (if successful :successful-trace :unsuccessful-trace)
                                                nil ;; No special functions
                                                (assoc trigger-trace
                                                       :corp-strength corp-strength
                                                       :runner-strength runner-strength
                                                       :successful successful
                                                       :corp-spent (if (corp-start? trace)
                                                                     (- strength base bonus)
                                                                     boost)
                                                       :runner-spent (if (corp-start? trace)
                                                                       boost
                                                                       (- strength link))))
                          (wait-for (resolve-ability state :corp (:eid which-ability) which-ability
                                                     card [corp-strength runner-strength])
                                    (if-let [kicker (:kicker trace)]
                                      (if (>= corp-strength (:kicker-min trace))
                                        (continue-ability state :corp kicker card [corp-strength runner-strength])
                                        (effect-completed state side eid))
                                      (effect-completed state side eid))))))))

(defn- trace-reply
  "Shows a trace prompt to the second player, after the first has already spent credits to boost."
  [state side eid card {:keys [player other base bonus link] :as trace} boost]
  (let [other-type (if (corp-start? trace) "link" "trace")
        strength (if (corp-start? trace)
                   ((fnil + 0 0 0) base bonus boost)
                   ((fnil + 0 0) link boost))
        trace (assoc trace :strength strength)]
    (wait-for (pay state player (make-eid state eid) card [:credit boost])
              (let [payment-str (:msg async-result)]
                (system-msg state player (str payment-str
                                              " to increase " (if (corp-start? trace) "trace" "link")
                                              " strength to " strength)))
              (clear-wait-prompt state other)
              (show-wait-prompt state player
                                (str (if (corp-start? trace) "Runner" "Corp")
                                     " to boost " other-type " strength"))
              (show-trace-prompt state other (make-eid state eid) card
                                 (str "Boost " other-type " strength?")
                                 #(resolve-trace state side eid card trace %)
                                 trace))))

(defn- trace-start
  "Starts the trace process by showing the boost prompt to the first player (normally corp)."
  [state side eid card {:keys [player other base bonus label] :as trace}]
  (let [this-type (if (corp-start? trace) "trace" "link")]
    (system-msg state player (str "uses " (:title card)
                                  " to initiate a trace with strength " ((fnil + 0 0) base bonus)
                                  (when (pos? bonus)
                                    (str " (" base " + " bonus ")"))
                                  (when label
                                    (str " (" label ")"))))
    (show-wait-prompt state other
                      (str (if (corp-start? trace) "Corp" "Runner")
                           " to boost " this-type " strength"))
    (show-trace-prompt state player (make-eid state eid) card
                       (str "Boost " this-type " strength?")
                       #(trace-reply state side eid card trace %)
                       trace)))

(defn- reset-trace-modifications
  [state]
  (swap! state assoc :trace nil))

(defn force-base
  [state value]
  (swap! state assoc-in [:trace :force-base] value))

(defn init-trace
  ([state side card] (init-trace state side (make-eid state {:source-type :trace}) card {:base 0}))
  ([state side card trace] (init-trace state side (make-eid state {:source-type :trace}) card trace))
  ([state side eid card {:keys [base] :as trace}]
   (reset-trace-modifications state)
   (wait-for (trigger-event-sync state :corp :initialize-trace card eid)
             (let [force-base (get-in @state [:trace :force-base])
                   force-link (first (get-effects state :corp :trace-force-link card [eid]))
                   base (cond force-base force-base
                              (fn? base) (base state :corp (make-eid state) card nil)
                              :else base)
                   link (or force-link (get-link state))
                   bonus (sum-effects state :corp :trace-base-strength card [eid])
                   initiator (determine-initiator state trace)
                   eid (assoc eid :source-type :trace)
                   corp-credits #(total-available-credits state :corp % card)
                   runner-credits #(total-available-credits state :runner % card)
                   trace (merge trace {:player initiator
                                       :other (if (= :corp initiator) :runner :corp)
                                       :base base
                                       :bonus bonus
                                       :link link
                                       :corp-credits corp-credits
                                       :runner-credits runner-credits})]
               (reset-trace-modifications state)
               (trace-start state side eid card trace)))))

(defn- check-trace
  "Checks if there is a trace to resolve"
  [state side {:keys [eid trace] :as ability} card targets]
  (assert (not (contains? trace :async)) "Put :async in the :successful/:unsuccessful")
  (if (can-trigger? state side eid ability card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :trace :req)
          (assoc :async true
                 :effect (effect (init-trace eid card trace))))
      card targets)
    (effect-completed state side eid)))

(register-ability-type :trace #'check-trace)
