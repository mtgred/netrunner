(ns netrunner.ajax
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [goog.net.XhrIo :as xhr]
            [goog.json :as json]))

(defn parse [event]
  (let [xhr (.-target event)
        status (.getStatus xhr)]
    (if (= status 200)
      {:status status :json (-> xhr .getResponseText ((.-parse js/JSON)) (js->clj :keywordize-keys true))}
      {:status status})))

(defn GET [url]
  (let [ch (chan)]
    (xhr/send url #(put! ch (parse %)))
    ch))

(defn POST
  ([url params]
     (let [ch (chan)]
       (xhr/send url #(put! ch (parse %)) "POST" params)
       ch))
  ([url params format]
     (let [ch (chan)
           headers (when (= format :json) #js {"Content-Type" "application/json"})
           content (if (= format :json) (json/serialize (clj->js params)) params)]
       (xhr/send url #(put! ch (parse %)) "POST" content headers)
       ch)))
