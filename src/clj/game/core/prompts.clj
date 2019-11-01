(ns game.core.prompts
  (:require [game.core.eid :refer [make-eid effect-completed]]
            [game.core.toasts :refer [toast]]))

(defn- add-to-prompt-queue
  "Adds a newly created prompt to the current prompt queue"
  [state side {:keys [priority] :as prompt}]
  (let [priority-comp #(case % true 1 nil 0 %)
        split-fn #(>= (priority-comp (:priority %)) (priority-comp priority))
        ;; insert the new prompt into the already-sorted queue based on its priority.
        update-fn #(let [[head tail] (split-with split-fn %)] (concat head (cons prompt tail)))]
    (swap! state update-in [side :prompt] update-fn)))

(defn show-prompt
  "Engine-private method for displaying a prompt where a *function*, not a card ability, is invoked
  when the prompt is resolved. All prompts flow through this method."
  ([state side card message choices f] (show-prompt state side (make-eid state) card message choices f nil))
  ([state side card message choices f args] (show-prompt state side (make-eid state) card message choices f args))
  ([state side eid card message choices f
    {:keys [priority prompt-type show-discard cancel-effect end-effect] :as args}]
   (let [prompt (if (string? message) message (message state side nil card nil))
         newitem {:eid eid
                  :msg prompt
                  :choices choices
                  :effect f
                  :card card
                  :prompt-type prompt-type
                  :show-discard show-discard
                  :priority priority
                  :cancel-effect cancel-effect
                  :end-effect end-effect}]
     (when (or (= prompt-type :waiting)
               (:number choices)
               (:card-title choices)
               (#{:credit :counter} choices)
               (pos? (count choices)))
       (add-to-prompt-queue state side newitem)))))

(defn show-prompt-with-dice
  "Calls show-prompt normally, but appends a 'roll d6' button to choices.
  If user chooses to roll d6, reveal the result to user and re-display
  the prompt without the 'roll d6 button'."
  ([state side card message other-choices f]
   (show-prompt state side card message other-choices f nil))
  ([state side card message other-choices f args]
   (let [dice-msg "Roll a d6",
         choices (conj other-choices dice-msg)]
     (show-prompt state side card message choices
                  #(if (not= % dice-msg)
                     (f %)
                     (show-prompt state side card
                                  (str message " (Dice result: " (inc (rand-int 6)) ")")
                                  other-choices f args))
                  args))))

(defn show-trace-prompt
  "Specific function for displaying a trace prompt. Works like `show-prompt` with some extensions.
   Always uses `:credit` as the `choices` variable, and passes on some extra properties, such as base and bonus."
  ([state side card message f args] (show-trace-prompt state side (make-eid state) card message f args))
  ([state side eid card message f {:keys [corp-credits runner-credits priority player other base bonus strength link] :as args}]
   (let [prompt (if (string? message) message (message state side nil card nil))
         corp-credits (corp-credits eid)
         runner-credits (runner-credits eid)
         newitem {:eid eid
                  :msg prompt
                  :choices (if (= :corp side) corp-credits runner-credits)
                  :corp-credits corp-credits
                  :runner-credits runner-credits
                  :prompt-type :trace
                  :effect f
                  :card card
                  :priority priority
                  :player player
                  :other other
                  :base base
                  :bonus bonus
                  :strength strength
                  :link link}]
     (add-to-prompt-queue state side newitem))))

(defn resolve-select
  "Resolves a selection prompt by invoking the prompt's ability with the targeted cards.
  Called when the user clicks 'Done' or selects the :max number of cards."
  [state side update! resolve-ability]
  (let [selected (get-in @state [side :selected 0])
        cards (map #(dissoc % :selected) (:cards selected))
        curprompt (first (get-in @state [side :prompt]))]
    (swap! state update-in [side :selected] #(vec (rest %)))
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % curprompt) pr)))
    (if-not (empty? cards)
      (do (doseq [card cards]
            (update! state side card))
          (resolve-ability state side (:ability selected) (:card curprompt) cards))
      (if-let [cancel-effect (:cancel-effect curprompt)]
        (cancel-effect nil)
        (effect-completed state side (:eid (:ability selected)))))))

(defn show-select
  "A select prompt uses a targeting cursor so the user can click their desired target of the ability.
  As with prompt!, the preferred method for showing a select prompt is through resolve-ability."
  ([state side card ability update! resolve-ability] (show-select state side card ability update! nil))
  ([state side card ability update! resolve-ability args]
   ;; if :max is a function, call it and assoc its return value as the new :max number of cards
   ;; that can be selected.
   (letfn [(wrap-function [args kw]
             (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
     (let [ability (update-in ability [:choices :max] #(if (fn? %) (% state side (make-eid state) card nil) %))
           all (get-in ability [:choices :all])
           m (get-in ability [:choices :max])]
       (swap! state update-in [side :selected]
              #(conj (vec %) {:ability (dissoc ability :choices)
                              :card (get-in ability [:choices :card])
                              :req (get-in ability [:choices :req])
                              :not-self (when (get-in ability [:choices :not-self]) (:cid card))
                              :max m
                              :all all}))
       (show-prompt state side card
                    (if-let [message (:prompt ability)]
                      message
                      (if m
                        (str "Select " (if all "" "up to ") m " targets for " (:title card))
                        (str "Select a target for " (:title card))))
                    (if all ["Hide"] ["Done"])
                    (if all
                      (fn [choice]
                        (toast state side (str "You must choose " m))
                        (show-select state side card ability args))
                      (fn [choice] (resolve-select state side update! resolve-ability)))
                    (-> args
                        (assoc :prompt-type :select
                               :show-discard (:show-discard ability))
                        (wrap-function :cancel-effect)))))))

(defn show-wait-prompt
  "Shows a 'Waiting for ...' prompt to the given side with the given message.
  The prompt cannot be closed except by a later call to clear-wait-prompt.
  The prompt has default priority 1, but can be overridden."
  ([state side message] (show-wait-prompt state side message {:priority 1}))
  ([state side message {:keys [priority card] :as args}]
   (show-prompt state side card (str "Waiting for " message) nil nil
                {:priority priority
                 :prompt-type :waiting})))

(defn clear-wait-prompt
  "Removes the first 'Waiting for...' prompt from the given side's prompt queue."
  [state side]
  (when-let [wait (some #(when (= :waiting (:prompt-type %)) %) (-> @state side :prompt))]
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % wait) pr)))))
