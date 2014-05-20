(ns netrunner.ajax
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [goog.net.XhrIo :as xhr]))

(defn GET [url]
  (let [ch (chan)]
    (xhr/send url (fn [event]
                    (let [response (-> event .-target .getResponseText JSON/parse
                                   (js->clj :keywordize-keys true))]
                      (put! ch response))))
    ch))

(defn POST [url params]
  (let [ch (chan)]
    (xhr/send url
              (fn [event]
                (let [xhr (.-target event)]
                  (if (= (.getStatus xhr) 200)
                    (put! ch (-> xhr .getResponseText JSON/parse (js->clj :keywordize-keys true)))
                    (put! ch {:error "Failed"}))))
              "POST" params)
    ch))
