(ns nr.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [jinteki.cards :refer [all-cards] :as cards]
            [jinteki.decks :as decks]
            [nr.appstate :refer [app-state]]
            [nr.account :refer [alt-art-name]]
            [nr.ajax :refer [GET]]
            [nr.utils :refer [toastr-options banned-span restricted-span rotated-span influence-dots]]

            ))

(defn make-span [text symbol class]
  (.replace text (js/RegExp. symbol "gi") (str "<span class='anr-icon " class "'></span>")))

(defn add-symbols [card-text]
  (-> (if (nil? card-text) "" card-text)
      (make-span "\\[Credits\\]" "credit")
      (make-span "\\[Credit\\]" "credit")
      (make-span "\\[Click\\]" "click")
      (make-span "\\[Subroutine\\]" "subroutine")
      (make-span "\\[Recurring Credits\\]" "recurring-credit")
      (make-span "\\[recurring-credit\\]" "recurring-credit")
      (make-span "1\\[Memory Unit\\]" "mu1")
      (make-span "2\\[Memory Unit\\]" "mu2")
      (make-span "3\\[Memory Unit\\]" "mu3")
      (make-span "\\[Memory Unit\\]" "mu")
      (make-span "1\\[mu\\]" "mu1")
      (make-span "2\\[mu\\]" "mu2")
      (make-span "3\\[mu\\]" "mu3")
      (make-span "\\[mu\\]" "mu")
      (make-span "\\[Link\\]" "link")
      (make-span "\\[Trash\\]" "trash")
      (make-span "\\[adam\\]" "adam")
      (make-span "\\[anarch\\]" "anarch")
      (make-span "\\[apex\\]" "apex")
      (make-span "\\[criminal\\]" "criminal")
      (make-span "\\[hb\\]" "haas-bioroid")
      (make-span "\\[haas-bioroid\\]" "haas-bioroid")
      (make-span "\\[jinteki\\]" "jinteki")
      (make-span "\\[nbn\\]" "nbn")
      (make-span "\\[shaper\\]" "shaper")
      (make-span "\\[sunny\\]" "sunny")
      (make-span "\\[weyland\\]" "weyland-consortium")
      (make-span "\\[weyland-consortium\\]" "weyland-consortium")))