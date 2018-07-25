(ns game.cards.resources
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))

(defn genetics-trigger?
  "Returns true if Genetics card should trigger - does not work with Adjusted Chronotype"
  [state side event]
  (or (first-event? state side event)
      (and (has-flag? state side :persistent :genetics-trigger-twice)
           (second-event? state side event))))

(defn shard-constructor
  "Function for constructing a Shard card"
  ([target-server message effect-fn] (shard-constructor target-server message nil effect-fn))
  ([target-server message ability-options effect-fn]
   (letfn [(can-install-shard? [state run] (and run
                                                (= (:server run) [target-server])
                                                (zero? (:position run))
                                                (not (:access @state))))]
     {:implementation "Click Shard to install when last ICE is passed, but before hitting Successful Run button"
      :abilities [(merge {:effect (effect (trash card {:cause :ability-cost}) (effect-fn eid card target))
                          :msg message}
                         ability-options)]
      :install-cost-bonus (req (when (can-install-shard? state run) [:credit -15 :click -1]))
      :effect (req (when (can-install-shard? state run)
                     (wait-for (register-successful-run state side (:server run))
                               (do (clear-wait-prompt state :corp)
                                   (swap! state update-in [:runner :prompt] rest)
                                   (handle-end-run state side)))))})))
