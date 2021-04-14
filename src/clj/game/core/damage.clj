(ns game.core.damage
  (:require
    [game.core.eid :refer [effect-completed make-eid make-result]]
    [game.core.engine :refer [trigger-event trigger-event-simult]]
    [game.core.flags :refer [cards-can-prevent? get-prevent-list]]
    [game.core.moving :refer [trash-cards]]
    [game.core.prompt-state :refer [add-to-prompt-queue remove-from-prompt-queue]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.winning :refer [flatline]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [dissoc-in]]
    [game.utils :refer [side-str]]
    [jinteki.utils :refer [str->int]]
    [clojure.string :as string]))

(defn damage-bonus
  "Registers a bonus of n damage to the next damage application of the given type."
  [state _ dtype n]
  (swap! state update-in [:damage :damage-bonus dtype] (fnil #(+ % n) 0)))

(defn prevention-prompt-msg
  [damage-amount damage-type prevented]
  (str "Prevent any of the " damage-amount
       " " (name damage-type) " damage?"
       " (" prevented "/" damage-amount " prevented)"))

(defn- damage-prevent-update-prompt
  "Look at the current runner prompt and (if a damage prevention prompt), update message."
  [state side]
  (when-let [prompt (first (get-in @state [side :prompt]))]
    (when-let [match (re-matches #"^Prevent any of the (\d+) (\w+) damage\?.*" (:msg prompt))]
      (let [damage-amount (str->int (second match))
            damage-type (case (nth match 2)
                          "net" :net
                          "brain" :brain
                          "meat" :meat)
            prevented (get-in @state [:damage :damage-prevent damage-type] 0)
            new-prompt (assoc prompt :msg (prevention-prompt-msg damage-amount
                                                                 damage-type
                                                                 prevented))]
        (remove-from-prompt-queue state side prompt)
        (if (>= prevented damage-amount)
          ((:effect prompt) nil)
          (add-to-prompt-queue state side new-prompt))))))

(defn damage-prevent
  "Registers a prevention of n damage to the next damage application of the given type. Afterwards update current prevention prompt, if found."
  [state side dtype n]
  (swap! state update-in [:damage :damage-prevent dtype] (fnil #(+ % n) 0))
  (damage-prevent-update-prompt state side))

(defn enable-runner-damage-choice
  [state _]
  (swap! state assoc-in [:damage :damage-choose-runner] true))

(defn enable-corp-damage-choice
  [state _]
  (swap! state assoc-in [:damage :damage-choose-corp] true))

(defn runner-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-runner]))

(defn corp-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-corp]))

(defn chosen-damage
  [state _ & targets]
  (swap! state update-in [:damage :chosen-damage] #(apply conj % (flatten targets))))

(defn- get-chosen-damage
  [state]
  (get-in @state [:damage :chosen-damage]))

(defn- damage-choice-priority
  "Determines which side gets to act if either or both have the ability to choose cards for damage.
  Currently just for Chronos Protocol vs Titanium Ribs"
  [state]
  (let [active-player (get-in @state [:active-player])]
    (when (and (corp-can-choose-damage? state) (runner-can-choose-damage? state))
      (if (= active-player :corp)
        (swap! state update-in [:damage] dissoc :damage-choose-runner)
        (swap! state update-in [:damage] dissoc :damage-choose-corp)))))

(defn- handle-replaced-damage
  [state side eid]
  (swap! state update-in [:damage :defer-damage] dissoc type)
  (swap! state update-in [:damage] dissoc :damage-replace)
  (effect-completed state side eid))

(defn- resolve-damage
  "Resolves the attempt to do n damage, now that both sides have acted to boost or
  prevent damage."
  [state side eid type n {:keys [card]}]
  (swap! state update-in [:damage :defer-damage] dissoc type)
  (swap! state dissoc-in [:damage :chosen-damage])
  (damage-choice-priority state)
  (wait-for (trigger-event-simult state side :pre-resolve-damage nil type side n)
            (if (get-in @state [:damage :damage-replace])
              (handle-replaced-damage state side eid)
              (if (pos? n)
                (let [hand (get-in @state [:runner :hand])
                      chosen-cards (seq (get-chosen-damage state))
                      chosen-cids (into #{} (map :cid chosen-cards))
                      leftovers (remove #(contains? chosen-cids (:cid %)) hand)
                      cards-trashed (filter identity (flatten (conj chosen-cards (seq (take (- n (count chosen-cards)) (shuffle leftovers))))))]
                  (when (= type :brain)
                    (swap! state update-in [:runner :brain-damage] #(+ % n)))
                  (when-let [trashed-msg (string/join ", " (map :title cards-trashed))]
                    (system-msg state :runner (str "trashes " trashed-msg " due to " (name type) " damage")))
                  (if (< (count hand) n)
                    (do (flatline state)
                        (swap! state update-in [:stats :corp :damage :all] (fnil + 0) n)
                        (swap! state update-in [:stats :corp :damage type] (fnil + 0) n)
                        (trash-cards state side eid cards-trashed {:unpreventable true}))
                    (wait-for (trash-cards state side cards-trashed {:unpreventable true :cause type})
                              (swap! state update-in [:stats :corp :damage :all] (fnil + 0) n)
                              (swap! state update-in [:stats :corp :damage type] (fnil + 0) n)
                              (trigger-event-simult state side
                                                    (make-result eid cards-trashed)
                                                    :damage nil type card n cards-trashed))))
                (effect-completed state side eid)))))

(defn damage-count
  "Calculates the amount of damage to do, taking into account prevention and boosting effects."
  [state _ dtype n {:keys [unpreventable unboostable]}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:damage :damage-bonus dtype])) 0))
      (- (or (when-not unpreventable (get-in @state [:damage :damage-prevent dtype])) 0))
      (max 0)))

(defn check-damage-prevention
  "for a preventable damage instance, handles all damage prevention effects that a player can use for it"
  ([state side eid type n args player]
   (let [interrupts (get-prevent-list state player type)
         other-player (if (= player :corp) :runner :corp)
         already-prevented (or (get-in @state [:damage :damage-prevent type]) 0)]
     (if (and (cards-can-prevent? state player interrupts type nil {:side side})
              (> n already-prevented))
       ;; player can prevent damage
       (do (system-msg state player "has the option to prevent damage")
           (show-wait-prompt state other-player (str (side-str player) " to prevent damage"))
           (swap! state assoc-in [:prevent :current] type)
           (show-prompt
             state player nil (str "Prevent any of the " (- n already-prevented) " " (name type) " damage?") ["Done"]
             (fn [_] (let [prevent (get-in @state [:damage :damage-prevent type])
                           damage-prevented (if prevent (- prevent already-prevented) false)]
                       (if damage-prevented (trigger-event state side :prevented-damage type prevent) nil)
                       (system-msg state player
                                   (if damage-prevented (str "prevents "
                                                             (if (>= damage-prevented Integer/MAX_VALUE) "all" damage-prevented)
                                                             " " (name type) " damage") "will not prevent damage"))
                       (clear-wait-prompt state other-player)
                       (effect-completed state side eid)))))
       (effect-completed state side eid)))))

(defn damage
  "Attempts to deal n damage of the given type to the runner. Starts the
  prevention/boosting process and eventually resolves the damage."
  ([state side eid type n] (damage state side eid type n nil))
  ([state side eid type n {:keys [unpreventable card] :as args}]
   (swap! state update-in [:damage :damage-bonus] dissoc type)
   (swap! state update-in [:damage :damage-prevent] dissoc type)
   ;; alert listeners that damage is about to be calculated.
   (trigger-event state side :pre-damage type card n)
   (let [active-player (get-in @state [:active-player])]
     (if unpreventable
       (resolve-damage state side eid type (damage-count state side type n args) args)
       (wait-for (check-damage-prevention state side type n args active-player)
                 (wait-for (check-damage-prevention state side type n args (if (= active-player :corp) :runner :corp))
                           (resolve-damage state side eid type (damage-count state side type n args) args)))))))
