(ns test.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid to-keyword capitalize
                                costs-to-symbol vdissoc distinct-by]]
            [game.macros :refer [effect req msg]]
            [clojure.string :refer [split-lines split join]]
            [game.core :as core]
            [test.utils :refer [load-card qty default-corp default-runner
                                make-deck]]
            [test.macros :refer [do-game]]
            [clojure.test :refer :all]))

(defn new-game [corp runner]
  (let [states (core/init-game
                 {:gameid 1
                  :players [{:side "Corp"
                             :deck {:identity (load-card (:identity corp))
                                    :cards (:deck corp)}}
                            {:side "Runner"
                             :deck {:identity (load-card (:identity runner))
                                    :cards (:deck runner)}}]})
        state (second (last states))]
    (core/keep-hand state :corp nil)
    (core/keep-hand state :runner nil)
    (core/start-turn state :corp nil)
    state))

(defn take-credits
  ([state side] (take-credits state side nil))
  ([state side n]
    (let [n (or n (if (= side :corp) 3 4))
          other (if (= side :corp) :runner :corp)]
      (dotimes [i n] (core/click-credit state side nil))
      (core/end-turn state side nil)
      (core/start-turn state other nil))))

(defn play-run-event [state card server]
  (core/play state :runner {:card card})
  (is (= [server] (get-in @state [:run :server])))
  (is (get-in @state [:run :run-effect]))
  (core/no-action state :corp nil)
  (core/successful-run state :runner nil)
  (is (get-in @state [:runner :prompt])) ; a prompt is being shown
  (is (get-in @state [:run :successful]))) ; the run was marked successful)

(defn find-card [title from]
  (some #(when (= (:title %) title) %) from))

(defn card-ability
  ([state side card ability] (card-ability state side card ability nil))
  ([state side card ability targets] (core/play-ability state side {:card card :ability ability :targets targets})))

(defn play-from-hand
  ([state side title] (play-from-hand state side title nil))
  ([state side title server]
    (core/play state side {:card (find-card title (get-in @state [side :hand]))
                           :server server})))

(load "cards")