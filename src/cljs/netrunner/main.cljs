(ns netrunner.main
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(.log js/console "foo")

(def app-state
  (atom
   {:game-state {:runner {}
                 :corp {}}}))

(defn deckbuilder-view [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil "Deck Builder"))))

(om/root deckbuilder-view app-state {:target (. js/document (getElementById "deckbuilder"))})

(defn gameboard-view [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil "Game board"))))

(om/root gameboard-view app-state {:target (. js/document (getElementById "gameboard"))})
