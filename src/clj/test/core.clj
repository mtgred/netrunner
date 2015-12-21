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
  "Init a new game using given corp and runner. Keep starting hands (no mulligan) and start Corp's turn."
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
  "Take credits for n clicks, or if no n given, for all remaining clicks of a side. If all clicks are used up,
  end turn and start the opponent's turn."
  ([state side] (take-credits state side nil))
  ([state side n]
    (let  [remaining-clicks (get-in @state [side :click])
           n (or n remaining-clicks)
           other (if (= side :corp) :runner :corp)]
      (dotimes [i n] (core/click-credit state side nil))
      (if (= (get-in @state [side :click]) 0)
        (do (core/end-turn state side nil)
            (core/start-turn state other nil))))))

(defn play-run-event [state card server]
  (core/play state :runner {:card card})
  (is (= [server] (get-in @state [:run :server])))
  (is (get-in @state [:run :run-effect]))
  (core/no-action state :corp nil)
  (core/successful-run state :runner nil)
  (is (get-in @state [:runner :prompt])) ; a prompt is being shown
  (is (get-in @state [:run :successful]))) ; the run was marked successful)

(defn find-card [title from]
  "Return a card with given title from given sequence"
  (some #(when (= (:title %) title) %) from))

(defn card-ability
  ([state side card ability] (card-ability state side card ability nil))
  ([state side card ability targets] (core/play-ability state side {:card (core/get-card state card)
                                                                    :ability ability :targets targets})))

(defn play-from-hand
  ([state side title] (play-from-hand state side title nil))
  ([state side title server]
    (core/play state side {:card (find-card title (get-in @state [side :hand]))
                           :server server})))

(defn score-agenda
  ([state side card]
   (let [title (get-in card [:title])
         advancementcost (get-in card [:advancementcost])]
    (core/gain state :corp :click advancementcost :credit advancementcost)
    (dotimes [n advancementcost]
      (core/advance state :corp {:card (core/get-card state card)}))
    (is (= advancementcost (get-in (core/get-card state card) [:advance-counter])))
    (core/score state :corp {:card (core/get-card state card)})
    (is (find-card title (get-in @state [:corp :scored])))
  )))

(defn last-log-contains?
  [state content]
  (not (nil?
         (re-find (re-pattern content)
                     (get (last (get-in @state [:log])) :text)))))

(defn trash-from-hand
  ([state side title]
   (core/trash state side (find-card title (get-in @state [side :hand])))))

(load "core-game")
(load "cards")