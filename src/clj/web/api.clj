(ns web.api
  (:require [web.utils :refer [response]]
            [web.data :as data]
            [web.pages :as pages]
            [web.auth :as auth]
            [web.ws :as ws]
            [web.game :as game]
            [web.chat :as chat]
            [web.stats :as stats]
            [web.admin :as admin]
            [web.tournament :as tournament]
            [web.news :as news]
            [web.decks :as decks]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-params wrap-json-response]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [ring.util.response :refer [resource-response]]
            [web.db :refer [db]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [compojure.core :refer [defroutes wrap-routes GET POST DELETE PUT]]))

(add-encoder org.bson.types.ObjectId encode-str)

(defroutes public-CSRF-routes
           (route/resources "/")
           (GET "/check-username/:username" [] auth/check-username-handler)
           (GET "/check-email/:email" [] auth/check-email-handler)

           (GET "/data/cards" [] data/cards-handler)
           (GET "/data/cards/version" [] data/cards-version-handler)
           (GET "/data/cards/altarts" [] data/alt-arts-handler)

           (GET "/data/news" [] news/news-handler)
           (GET "/data/sets" [] data/sets-handler)
           (GET "/data/mwl" [] data/mwl-handler)
           (GET "/data/cycles" [] data/cycles-handler)
           (GET "/data/donors" [] data/donors-handler)

           (GET "/chat/config" [] chat/config-handler)
           (GET "/messages/:channel" [] chat/messages-handler)

           (GET "/reset/:token" [] pages/reset-password-page)

           (GET "/ws" req ws/handshake-handler)
           (POST "/ws" req ws/post-handler)

           (GET "/*" [] pages/index-page))

(defroutes public-routes
           (POST "/register" [] auth/register-handler)
           (POST "/login" [] auth/login-handler)
           (POST "/forgot" [] auth/forgot-password-handler)
           (POST "/reset/:token" [] auth/reset-password-handler))

(defroutes admin-routes
           (GET "/admin/announce" [] pages/announce-page)
           (POST "/admin/announce" [] admin/announcement-handler)
           (GET "/admin/version" [] pages/version-page)
           (POST "/admin/version" [] admin/version-handler)
           (GET "/admin/fetch" [] pages/fetch-page)
           (POST "/admin/fetch" [] admin/fetch-handler))

(defroutes user-routes
           (POST "/logout" [] auth/logout-handler)
           (PUT "/profile" [] auth/update-profile-handler)
           (GET "/profile/email" [] auth/email-handler)
           (PUT "/profile/email" [] auth/change-email-handler)

           (DELETE "/profile/stats/user" [] stats/clear-userstats-handler)
           (DELETE "/profile/stats/deck/:id" [] stats/clear-deckstats-handler)

           (GET "/profile/history" [] stats/history)
           (GET "/profile/history/:gameid" [] stats/fetch-log)

           (GET "/data/decks" [] decks/decks-handler)
           (POST "/data/decks" [] decks/decks-create-handler)
           (PUT "/data/decks" [] decks/decks-save-handler)
           (DELETE "/data/decks/:id" [] decks/decks-delete-handler))

(defroutes tournament-routes
  (GET "/tournament-auth/:username" [] tournament/auth))

(defroutes private-routes
  (wrap-routes user-routes auth/wrap-authentication-required)
  (wrap-routes tournament-routes auth/wrap-tournament-auth-required)
  (wrap-routes admin-routes auth/wrap-authorization-required))

(defroutes routes
  (wrap-routes private-routes wrap-anti-forgery)
  (wrap-routes public-CSRF-routes wrap-anti-forgery)
  public-routes)

(defn wrap-return-favicon [handler]
  (fn [req]
    (if (= [:get "/favicon.ico"] [(:request-method req) (:uri req)])
      (resource-response "jinteki.ico" {:root "public/img"})
      (handler req))))

(def app
  (-> routes
      auth/wrap-user
      wrap-keyword-params
      wrap-params
      wrap-json-response
      wrap-session
      (wrap-json-body {:keywords? true})
      admin/wrap-version
      wrap-return-favicon
      wrap-stacktrace))
