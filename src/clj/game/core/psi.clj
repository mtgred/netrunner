(ns game.core.psi
  (:require
    [game.core.card :refer [corp?]]
    [game.core.costs :refer [total-available-credits]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [can-trigger? pay register-ability-type register-once resolve-ability trigger-event-simult]]
    [game.core.flags :refer [any-flag-fn?]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt-with-dice show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [continue-ability effect wait-for]]
    [jinteki.utils :refer [str->int]]
    [clojure.string :as string]
    [game.core.payment :refer [->c]]))

(defn- resolve-psi
  "Resolves a psi game by charging credits to both sides and invoking the appropriate
  resolution ability."
  [state side eid card psi bet targets]
  (swap! state assoc-in [:psi side] bet)
  (let [opponent (if (= side :corp) :runner :corp)]
    (if-let [opponent-bet (get-in @state [:psi opponent])]
      (wait-for
        (pay state opponent (make-eid state eid) card [(->c :credit opponent-bet)])
        (system-msg state opponent (:msg async-result))
        (wait-for
          (pay state side (make-eid state eid) card (->c :credit bet))
          (system-msg state side (:msg async-result))
          (clear-wait-prompt state opponent)
          (wait-for (trigger-event-simult state side (make-eid state eid) :reveal-spent-credits nil (get-in @state [:psi :corp]) (get-in @state [:psi :runner]))
                    (if-let [ability (if (= bet opponent-bet) (:equal psi) (:not-equal psi))]
                      (let [card-side (if (corp? card) :corp :runner)]
                        (continue-ability state card-side (assoc ability :async true) card targets))
                      (effect-completed state side eid)))))
      (show-wait-prompt
        state side (str (string/capitalize (name opponent)) " to choose psi game credits")))))

(defn psi-game
  "Starts a psi game by showing the psi prompt to both players. psi is a map containing
  :equal and :not-equal abilities which will be triggered in resolve-psi accordingly."
  ([state side card psi] (psi-game state side (make-eid state {:source-type :psi}) card psi nil))
  ([state side eid card psi targets]
   (swap! state assoc :psi {})
   (register-once state side psi card)
   (let [eid (assoc eid :source-type :psi)]
     (doseq [s [:corp :runner]]
       (let [all-amounts (range (min 3 (inc (total-available-credits state s eid card))))
             valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                        (any-flag-fn? state :runner :prevent-secretly-spend %))
                                   all-amounts)]
         (show-prompt-with-dice state s card (str "Choose an amount to spend for " (:title card))
                                (map #(str % " [Credits]") valid-amounts)
                                #(resolve-psi state s eid card psi (str->int (first (string/split (:value %) #" "))) targets)
                                {:prompt-type :psi}))))))

(defn- check-psi
  "Checks if a psi-game is to be resolved"
  [state side {:keys [eid psi] :as ability} card targets]
  (assert (not (contains? psi :async)) "Put :async in the :equal/:not-equal.")
  (if (can-trigger? state side eid psi card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :psi :once :req)
          (assoc :async true
                 :effect (effect (psi-game eid card psi targets))))
      card targets)
    (effect-completed state side eid)))

(register-ability-type :psi #'check-psi)
