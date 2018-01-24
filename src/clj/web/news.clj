(ns web.news
  (:require [trello.core :refer [make-client]]
            [trello.client :as trello]
            [web.config :refer [server-config]]
            [web.utils :refer [response]]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(defonce trello-auth (:trello-auth server-config))
(defonce news-items (atom {}))
(defonce trello-time-formatter (f/formatters :date-time))

(defn fetch-news []
  (try
    (let [client (make-client (:key trello-auth) (:token trello-auth))
          cards (client trello/get (str "/lists/" (:list-id trello-auth) "/cards")
                        :params {:filter "open" :fields "dateLastActivity,name,labels"})]
      {:time (t/now)
       :news (for [c (filter #(empty? (:labels %)) cards)]
               {:title (:name c)
                :date  (f/parse trello-time-formatter (:dateLastActivity c))})})

    (catch Exception e
      (prn "Exception in news fetch" e))))

(defn get-news []
  (let [{:keys [time news]} @news-items
        now (t/now)]
    (when (or (not news)
              (not (t/within? (t/interval time (t/plus time (t/minutes 1))) now)))
      (reset! news-items (fetch-news)))
    (:news @news-items)))

(defn news-handler [request]
  (if (:key trello-auth)
    (response 200 (get-news))
    (response 200 [{:date "01/01/2015 00:00" :title "DEVELOPER: Get a Trello API Key and Token, and set the :trello-auth variables in config.edn to see the news."}])))
