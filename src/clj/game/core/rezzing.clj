(ns game.core.rezzing
  (:require
    [game.core.card :refer [asset? get-card ice? upgrade?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [rez-additional-cost-bonus rez-cost]]
    [game.core.effects :refer [unregister-constant-effects]]
    [game.core.eid :refer [complete-with-result effect-completed eid-set-defaults make-eid]]
    [game.core.engine :refer [ability-as-handler card-as-handler pay register-events resolve-ability trigger-event trigger-event-simult unregister-events]]
    [game.core.flags :refer [can-rez?]]
    [game.core.ice :refer [update-ice-strength]]
    [game.core.initializing :refer [card-init deactivate]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs]]
    [game.core.runs :refer [continue]]
    [game.core.say :refer [play-sfx system-msg]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability effect wait-for]]
    [game.utils :refer [to-keyword]]))

(defn get-rez-cost
  [state side card {:keys [ignore-cost alternative-cost cost-bonus]}]
  (merge-costs
    (cond
      (= :all-costs ignore-cost) [:credit 0]
      alternative-cost alternative-cost
      :else (let [cost (rez-cost state side card {:cost-bonus cost-bonus})
                  additional-costs (rez-additional-cost-bonus state side card)]
              (concat
                (when-not ignore-cost
                  [:credit cost])
                (when (not (:disabled card))
                  additional-costs))))))

(defn- complete-rez
  [state side eid
   {:keys [disabled] :as card}
   {:keys [alternative-cost ignore-cost no-warning no-msg press-continue] :as args}]
  (let [cdef (card-def card)
        costs (get-rez-cost state side card args)]
    (wait-for (pay state side (make-eid state eid) card costs)
              (if-let [payment-str (:msg async-result)]
                (let [_ (when (:derezzed-events cdef)
                          (unregister-events state side card))
                      card (if-not disabled
                             (card-init state side (assoc card :rezzed :this-turn) {:resolve-effect false :init-data true})
                             (update! state side (assoc card :rezzed :this-turn)))
                      card-ability (if-let [ability (:on-rez cdef)]
                                     (ability-as-handler card ability)
                                     (card-as-handler card))]
                  (doseq [h (:hosted card)]
                    (update! state side (-> h
                                            (update-in [:zone] #(map to-keyword %))
                                            (update-in [:host :zone] #(map to-keyword %)))))
                  (when-not no-msg
                    (system-msg state side
                                (str (build-spend-msg payment-str "rez" "rezzes")
                                     (:title card)
                                     (cond
                                       alternative-cost " by paying its alternative cost"
                                       ignore-cost " at no cost"))))
                  (when (and (not no-warning) (:corp-phase-12 @state))
                    (toast state :corp "You are not allowed to rez cards between Start of Turn and Mandatory Draw.
                                       Please rez prior to clicking Start Turn in the future." "warning"
                           {:time-out 0 :close-button true}))
                  (if (ice? card)
                    (do (update-ice-strength state side card)
                        (play-sfx state side "rez-ice"))
                    (play-sfx state side "rez-other"))
                  (swap! state update-in [:stats :corp :cards :rezzed] (fnil inc 0))
                  (wait-for (trigger-event-simult state :corp (make-eid state eid) :rez {:card-abilities card-ability} (get-card state card))
                            (when press-continue
                              (continue state side nil))
                            (complete-with-result state side eid {:card (get-card state card)})))
                (effect-completed state side eid)))))

(defn rez
  "Rez a corp card."
  ([state side eid card] (rez state side eid card nil))
  ([state side eid card
    {:keys [ignore-cost force declined-alternative-cost alternative-cost] :as args}]
   (let [eid (eid-set-defaults eid :source nil :source-type :rez)
         card (get-card state card)
         alternative-cost (when (and card
                                     (not alternative-cost)
                                     (not declined-alternative-cost))
                            (:alternative-cost (card-def card)))]
     (if (and card
              (or force
                  (can-rez? state side card))
              (or (asset? card)
                  (ice? card)
                  (upgrade? card)
                  (:install-rezzed (card-def card))))
       (if (and alternative-cost
                (not ignore-cost)
                (can-pay? state side eid card nil alternative-cost))
         (continue-ability
           state side
           {:optional
            {:prompt "Pay the alternative Rez cost?"
             :yes-ability {:async true
                           :effect (effect (rez eid card (merge args {:ignore-cost true
                                                                      :alternative-cost alternative-cost})))}
             :no-ability {:async true
                          :effect (effect (rez eid card (merge args {:declined-alternative-cost true})))}}}
           card nil)
         (complete-rez state side eid card args))
       (effect-completed state side eid)))))

;; TODO: make async
(defn derez
  "Derez a corp card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "derezzes " (:title card)))
    (unregister-events state side card)
    (update! state :corp (deactivate state :corp card true))
    (let [cdef (card-def card)]
      (when-let [derez-effect (:derez-effect cdef)]
        (resolve-ability state side derez-effect (get-card state card) nil))
      (when-let [derezzed-events (:derezzed-events cdef)]
        (register-events state side card (map #(assoc % :condition :derezzed) derezzed-events))))
    (unregister-constant-effects state side card)
    (trigger-event state side :derez card side)))
