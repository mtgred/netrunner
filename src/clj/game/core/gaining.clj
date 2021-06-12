(ns game.core.gaining
  (:require
    [game.core.eid :refer [make-eid effect-completed]]
    [game.core.engine :refer [trigger-event trigger-event-sync trigger-event-simult]]
    [game.macros :refer [effect-seq wait-for]]
    [game.core.toasts :refer [toast]]))

(defn safe-inc-n
  "Helper function to safely update a value by n. Returns a function to use with `update` / `update-in`"
  [n]
  (partial (fnil + 0 0) n))

(defn sub->0
  "Helper function for use in `update` or `update-in` to subtract for a value, to a minimum of 0."
  [n]
  #(max 0 ((fnil - 0 0) % n)))

(defn deduct
  "Deduct the value from the player's attribute."
  [state side [attr value]]
  (cond
    ;; value is a map, should be :base, :mod, etc.
    (map? value)
    (doseq [[subattr value] value]
      (swap! state update-in [side attr subattr] (if (#{:mod :used} subattr)
                                                   ;; Modifications and mu used may be negative
                                                   ;; mu used is for easier implementation of the 0-mu hosting things
                                                   #(- % value)
                                                   (sub->0 value))))

    ;; values that expect map, if passed a number use default subattr of :mod
    (#{:memory} attr)
    (deduct state side [attr {:mod value}])

    ;; default case for tags and bad-publicity is `:base`
    (#{:tag :bad-publicity} attr)
    (deduct state side [attr {:base value}])

    :else
    (do (swap! state update-in [side attr] (if (= attr :agenda-point)
                                             ;; Agenda points may be negative
                                             #(- % value)
                                             (sub->0 value)))
        (when (and (= attr :credit)
                   (= side :runner)
                   (pos? (get-in @state [:runner :run-credit] 0)))
          (swap! state update-in [:runner :run-credit] (sub->0 value))))))

(defn gain [state side eid & args]
  (effect-seq state side eid [[cost-type amount] (partition 2 args)]
    (let [continuation #(trigger-event-simult state side eid (if (= side :corp) :corp-gain :runner-gain) nil [cost-type amount])]
      (cond
        ;; amount is a map, merge-update map
        (map? amount)
        (do (doseq [[subtype amount] amount]
              (swap! state update-in [side cost-type subtype] (safe-inc-n amount))
              (swap! state update-in [:stats side :gain cost-type subtype] (fnil + 0) amount))
            (continuation))

        ;; Default cases for the types that expect a map
        (#{:hand-size :memory} cost-type)
        (gain state side eid cost-type {:mod amount})

        ;; Default case for tags and bad publicity is `:base`
        (#{:tag :bad-publicity} cost-type)
        (gain state side eid cost-type {:base amount})

        ;; Else assume amount is a number and try to increment cost-type by it.
        :else
        (do (swap! state update-in [side cost-type] (safe-inc-n amount))
            (swap! state update-in [:stats side :gain cost-type] (fnil + 0 0) amount)
            (continuation))))))

(defn lose [state side eid & args]
 (effect-seq state side eid [[cost-type amount] (partition 2 args)]
   (if (= amount :all)
     (do (swap! state update-in [:stats side :lose cost-type] (fnil + 0) (get-in @state [side cost-type]))
         (swap! state assoc-in [side cost-type] 0))
     (do (when (number? amount)
           (swap! state update-in [:stats side :lose cost-type] (fnil + 0) amount))
         (deduct state side [cost-type amount])))
   (trigger-event-simult state side eid (if (= side :corp) :corp-lose :runner-lose) nil [cost-type amount])))

(defn gain-credits
  "Utility function for triggering events"
  ([state side eid amount] (gain-credits state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (do (wait-for (gain state side (make-eid state eid) :credit amount)
           (trigger-event-simult state side eid (if (= :corp side) :corp-credit-gain :runner-credit-gain) nil amount args)))
     (effect-completed state side eid))))

(defn lose-credits
  "Utility function for triggering events"
  ([state side eid amount] (lose-credits state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (or (= :all amount)
                (pos? amount)))
     (wait-for (lose state side (make-eid state eid) :credit amount)
         (if (and (= side :runner)
                    (= :all amount))
           (wait-for (lose state :runner (make-eid state eid) :run-credit :all)
             (trigger-event-simult state side eid (if (= :corp side) :corp-credit-loss :runner-credit-loss) nil amount args))
         (trigger-event-simult state side eid (if (= :corp side) :corp-credit-loss :runner-credit-loss) nil amount args)))
     (effect-completed state side eid))))

;;; Stuff for handling {:base x :mod y} data structures
(defn base-mod-size
  "Returns the value of properties using the `base` and `mod` system"
  [state side prop]
  (let [base (get-in @state [side prop :base] 0)
        mod (get-in @state [side prop :mod] 0)]
    (+ base mod)))
