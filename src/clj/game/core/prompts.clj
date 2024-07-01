(ns game.core.prompts
  (:require
    [clj-uuid :as uuid]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.prompt-state :refer [add-to-prompt-queue remove-from-prompt-queue]]
    [game.core.toasts :refer [toast]]
    [game.macros :refer [when-let*]]
    [game.utils :refer [pluralize side-str]]
    [medley.core :refer [find-first]]))

(defn choice-parser
  [choices]
  (if (or (map? choices) (keyword? choices))
    choices
    (into
      []
      (for [[idx choice] (map-indexed vector (keep identity choices))]
        {:value choice
         :uuid (uuid/v4)
         :idx idx}))))

(defn show-prompt
  "Engine-private method for displaying a prompt where a *function*, not a card ability, is invoked
  when the prompt is resolved. All prompts flow through this method."
  ([state side card message choices f] (show-prompt state side (make-eid state) card message choices f nil))
  ([state side card message choices f args] (show-prompt state side (make-eid state) card message choices f args))
  ([state side eid card message choices f
    {:keys [waiting-prompt prompt-type show-discard cancel-effect end-effect targets]}]
   (let [prompt (if (string? message) message (message state side eid card targets))
         choices (choice-parser choices)
         newitem {:eid eid
                  :msg prompt
                  :choices choices
                  :effect f
                  :card card
                  :prompt-type (or prompt-type :other)
                  :show-discard show-discard
                  :cancel-effect cancel-effect
                  :end-effect end-effect}]
     (when (or (#{:waiting :run} prompt-type)
               (:number choices)
               (:card-title choices)
               (#{:credit :counter} choices)
               (pos? (count choices)))
       (when waiting-prompt
         (add-to-prompt-queue
           state (if (= :corp side) :runner :corp)
           {:eid (select-keys eid [:eid])
            :card card
            :prompt-type :waiting
            :msg (str "Waiting for " 
                      (if (true? waiting-prompt)
                        (str (side-str side) " to make a decision")
                        waiting-prompt))}))
       (add-to-prompt-queue state side newitem)))))

(defn show-prompt-with-dice
  "Calls show-prompt normally, but appends a 'roll d6' button to choices.
  If user chooses to roll d6, reveal the result to user and re-display
  the prompt without the 'roll d6 button'."
  ([state side card message other-choices f]
   (show-prompt-with-dice state side card message other-choices f nil))
  ([state side card message other-choices f args]
   (let [dice-msg "Roll a d6",
         choices (conj other-choices dice-msg)]
     (show-prompt state side card message choices
                  #(if (not= (:value %) dice-msg)
                     (f %)
                     (show-prompt state side card
                                  (str message " (Dice result: " (inc (rand-int 6)) ")")
                                  other-choices f args))
                  args))))

(defn show-trace-prompt
  "Specific function for displaying a trace prompt. Works like `show-prompt` with some extensions.
   Always uses `:credit` as the `choices` variable, and passes on some extra properties, such as base and bonus."
  ([state side card message f args] (show-trace-prompt state side (make-eid state) card message f args))
  ([state side eid card message f {:keys [corp-credits runner-credits player other base bonus strength link targets]}]
   (let [prompt (if (string? message) message (message state side eid card targets))
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
  [state side card args update! resolve-ability]
  (let [selected (get-in @state [side :selected 0])
        cards (map #(dissoc % :selected) (:cards selected))
        prompt (first (filter #(= :select (:prompt-type %)) (get-in @state [side :prompt])))]
    (swap! state update-in [side :selected] #(vec (rest %)))
    (when prompt
      (remove-from-prompt-queue state side prompt))
    (if (seq cards)
      (do (doseq [card cards]
            (update! state side card))
          (resolve-ability state side (:ability selected) card cards))
      (if-let [cancel-effect (:cancel-effect args)]
        (cancel-effect nil)
        (effect-completed state side (:eid (:ability selected)))))))

(defn show-select
  "A select prompt uses a targeting cursor so the user can click their desired target of the ability.
  The preferred method for showing a select prompt is through resolve-ability."
  ([state side card ability update! resolve-ability args]
   ;; if :max or :min are a function, call them and assoc its return value as the new :max / :min number of cards
   ;; that can be selected.
   (letfn [(wrap-function [args kw]
             (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
     (let [targets (:targets args)
           ability (update-in ability [:choices :max] #(if (fn? %) (% state side (make-eid state) card targets) %))
           ability (update-in ability [:choices :min] #(if (fn? %) (% state side (make-eid state) card targets) %))
           all (get-in ability [:choices :all])
           ability (if all (update-in ability [:choices] dissoc :min) ability) ; ignore :min if :all is set
           min-choices (get-in ability [:choices :min])
           max-choices (get-in ability [:choices :max])]
       (swap! state update-in [side :selected]
              #(conj (vec %) {:ability (-> ability
                                           (dissoc :choices :waiting-prompt)
                                           (assoc :card card))
                              :cards []
                              :card (get-in ability [:choices :card])
                              :req (get-in ability [:choices :req])
                              :not-self (when (get-in ability [:choices :not-self]) (:cid card))
                              :max max-choices
                              :all all}))
       (show-prompt state side (:eid ability)
                    card
                    (if-let [message (:prompt ability)]
                      message
                      (str
                        "Choose"
                        (when min-choices
                          (str " at least " min-choices))
                        (when (and min-choices max-choices)
                          " and")
                        (when max-choices
                          (str (if all "" " up to")
                               " " max-choices))
                        (if max-choices
                          (str " " (pluralize "target" max-choices))
                          (if min-choices
                            (str " " (pluralize "target" min-choices))
                            " a target"))
                        " for " (:title card)))
                    (if all ["Hide"] ["Done"])
                    (if all
                      (fn [_]
                        ; "Hide" was selected. Show toast and reapply select prompt. This allows players to access
                        ; prompts that lie "beneath" the current select prompt.
                        (toast state side (str "You must choose " max-choices " " (pluralize "card" max-choices)))
                        (show-select state side card ability update! resolve-ability args))
                      (fn [_]
                        (let [selected (get-in @state [side :selected 0] [])
                              cards (map #(dissoc % :selected) (:cards selected))]
                          ; check for :min. If not enough cards are selected, show toast and stay in select prompt
                          (if (and min-choices (< (count cards) min-choices))
                            (do
                              (toast state side (str "You must choose at least " min-choices " " (pluralize "card" min-choices)))
                              (show-select state side card ability update! resolve-ability args))
                            (resolve-select state side card
                                            (select-keys (wrap-function args :cancel-effect) [:cancel-effect])
                                            update! resolve-ability)))))
                    (-> args
                        (assoc :prompt-type :select
                               :show-discard (:show-discard ability))
                        (wrap-function :cancel-effect)))))))

(defn show-wait-prompt
  "Shows a 'Waiting for ...' prompt to the given side with the given message.
  The prompt cannot be closed except by a later call to clear-wait-prompt."
  ([state side message] (show-wait-prompt state side message nil))
  ([state side message {:keys [card]}]
   (show-prompt state side card (str "Waiting for " message) nil nil
                {:prompt-type :waiting})))

(defn clear-wait-prompt
  "Removes the first 'Waiting for...' prompt from the given side's prompt queue."
  [state side]
  (when-let [wait (find-first #(= :waiting (:prompt-type %)) (-> @state side :prompt))]
    (remove-from-prompt-queue state side wait)))

(defn show-run-prompts
  "Adds a dummy prompt to both side's prompt queues.
   The prompt cannot be closed except by a later call to clear-run-prompts."
  [state msg card]
  (show-prompt state :runner card (str "You are " msg) nil nil {:prompt-type :run})
  (show-prompt state :corp card (str "The Runner is " msg) nil nil {:prompt-type :run}))

(defn clear-run-prompts
  [state]
  (when-let* [runner-prompt (find-first #(= :run (:prompt-type %)) (-> @state :runner :prompt))
              corp-prompt (find-first #(= :run (:prompt-type %)) (-> @state :corp :prompt))]
             (remove-from-prompt-queue state :runner runner-prompt)
             (remove-from-prompt-queue state :corp corp-prompt)))

(defn cancellable
  "Wraps a vector of prompt choices with a final 'Cancel' option. Optionally sorts the vector alphabetically,
  with Cancel always last."
  ([choices] (cancellable choices false))
  ([choices sorted]
   (if sorted
     (conj (vec (sort-by :title choices)) "Cancel")
     (conj (vec choices) "Cancel"))))
