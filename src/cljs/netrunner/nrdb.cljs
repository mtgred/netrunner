(ns netrunner.nrdb
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.deckbuilder :refer [process-decks num->percent]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.ajax :refer [POST GET]]
            [goog.string :as gstring]
            [goog.string.format]))

(go (swap! app-state assoc :nrdb_auth_url (:auth_url (:json (<! (GET "/nrdb/config"))))))

(defn nrdb [{:keys [nrdb_auth_url] :as cursor} owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (sab/html
        [:div.blue-shade.content-page.panel
         [:h3 "NetrunnerDB Integration"]
         [:a {:href nrdb_auth_url} "Authorize"]
         ]))))

(om/root nrdb app-state {:target (. js/document (getElementById "nrdb"))})
