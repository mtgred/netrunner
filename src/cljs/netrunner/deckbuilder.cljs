(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.socket :refer [out-channel chat-channel]]))

(defn deckbuilder-view [app owner]
  (om/component
   (sab/html [:h1 {} "Deck Builder"])))

(om/root deckbuilder-view app-state {:target (. js/document (getElementById "deckbuilder"))})
