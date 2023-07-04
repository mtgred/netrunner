(ns game.core.finding
  (:require [game.core.board :refer [all-installed]]
            [game.utils :refer [to-keyword]]))

;;; Functions for loading card information.
(defn find-card
  "Return a card with given title from given sequence"
  [title from]
  (some #(when (= (:title %) title) %) from))

(defn find-cid
  "Return a card with specific :cid from given sequence"
  [cid from]
  (some #(when (= (:cid %) cid) %) from))

(defn find-latest
  "Returns the newest version of a card where-ever it may be"
  [state card]
  (find-cid (:cid card) (concat (all-installed state (-> card :side to-keyword))
                                (-> (map #(get-in @state [:corp %]) [:hand :discard :deck :rfg :scored]) concat flatten)
                                (-> (map #(get-in @state [:runner %]) [:hand :discard :deck :rfg :scored]) concat flatten))))

(defn get-scoring-owner
  "Returns the owner of the scoring area the card is in"
  [state {:keys [cid]}]
  (cond
    (find-cid cid (get-in @state [:corp :scored]))
    :corp
    (find-cid cid (get-in @state [:runner :scored]))
    :runner))
