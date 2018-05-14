(ns nr.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [jinteki.cards :refer [all-cards] :as cards]
            [jinteki.decks :as decks]
            [nr.appstate :refer [app-state]]
           ; [nr.account :refer [alt-art-name]]
           ; [nr.ajax :refer [GET]]
            [nr.utils :refer [toastr-options banned-span restricted-span rotated-span influence-dots]]

            ))