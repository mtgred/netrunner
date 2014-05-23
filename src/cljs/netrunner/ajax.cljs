(ns netrunner.ajax
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [goog.net.XhrIo :as xhr]))

(defn parse [event]
  (-> event .-target .getResponseText JSON/parse (js->clj :keywordize-keys true)))

(defn GET [url]
  (let [ch (chan)]
    (xhr/send url #(put! ch (parse %)))
    ch))

(defn POST [url params]
  (let [ch (chan)]
    (xhr/send url #(let [xhr (.-target %)
                         status (.getStatus xhr)]
                     (if (= status 200)
                       (put! ch {:status status :json (parse %)})
                       (put! ch {:status status})))
              "POST" params)
    ch))
