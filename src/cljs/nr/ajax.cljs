(ns nr.ajax
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [goog.net.XhrIo :as xhr]
            [goog.json :as json]))

(def ?csrf-token
  (when-let [el (.getElementById js/document "sente-csrf-token")]
    (.getAttribute el "data-csrf-token")))

(defn parse [event]
  (let [xhr (.-target event)
        status (.getStatus xhr)]
    {:status status
     :json (-> xhr .getResponseText ((.-parse js/JSON)) (js->clj :keywordize-keys true))}))

(defn GET [url]
  (let [ch (chan)]
    (xhr/send url #(put! ch (parse %)))
    ch))

(defn DELETE
  [url]
  (let [ch (chan)]
    (xhr/send url #(put! ch (parse %)) "DELETE" "" #js {"X-CSRF-Token" ?csrf-token})
    ch))

(defn POST
  ([url params]
   (let [ch (chan)]
     (xhr/send url #(put! ch (parse %)) "POST" params #js {"X-CSRF-Token" ?csrf-token})
     ch))
  ([url params format]
   (let [ch (chan)
         headers (if (= format :json) #js {"Content-Type" "application/json" "X-CSRF-Token" ?csrf-token} #js {"X-CSRF-Token" ?csrf-token})
         content (if (= format :json) (json/serialize (clj->js params)) params)]
     (xhr/send url #(put! ch (parse %)) "POST" content headers)
     ch)))

(defn PUT
  ([url params]
   (let [ch (chan)]
     (xhr/send url #(put! ch (parse %)) "UPDATE" params #js {"X-CSRF-Token" ?csrf-token})
     ch))
  ([url params format]
   (let [ch (chan)
         headers (if (= format :json) #js {"Content-Type" "application/json" "X-CSRF-Token" ?csrf-token} #js {"X-CSRF-Token" ?csrf-token})
         content (if (= format :json) (json/serialize (clj->js params)) params)]
     (xhr/send url #(put! ch (parse %)) "PUT" content headers)
     ch)))
