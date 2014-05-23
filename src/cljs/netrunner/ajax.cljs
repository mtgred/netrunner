(ns netrunner.ajax
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [goog.net.XhrIo :as xhr]))

(defn parse [event]
  (let [xhr (.-target event)
        status (.getStatus xhr)
        response (-> xhr .getResponseText JSON/parse (js->clj :keywordize-keys true))]
    {:status status :json response}))

(defn GET [url]
  (let [ch (chan)]
    (xhr/send url #(put! ch (parse %)))
    ch))

(defn POST [url params]
  (let [ch (chan)]
    (xhr/send url #(put! ch (parse %)) "POST" params)
    ch))
