(ns web.api
  (:require [web.data :as data]
            [web.pages :as pages]
            [web.auth :as auth]
            [web.ws :as ws]
            [web.chat :as chat]
            [web.stats :as stats]
            [web.angel-arena :as angel-arena]
            [web.admin :as admin]
            [web.tournament :as tournament]
            [web.decks :as decks]
            [web.api-keys :as api-keys]
            [web.game-api :as game-api]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.util.response :refer [resource-response]]
            [puppetlabs.ring-middleware.core :refer [wrap-add-cache-headers]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [compojure.core :refer [defroutes wrap-routes GET POST DELETE PUT]]))

(add-encoder org.bson.types.ObjectId encode-str)

(defroutes public-CSRF-routes
           (GET "/check-username/:username" [] #'auth/check-username-handler)
           (GET "/check-email/:email" [] #'auth/check-email-handler)

           (GET "/data/cards" [] #'data/cards-handler)
           (GET "/data/cards/version" [] #'data/cards-version-handler)
           (GET "/data/cards/altarts" [] #'data/alt-arts-handler)

           (GET "/data/news" [] #'data/news-handler)
           (GET "/data/sets" [] #'data/sets-handler)
           (GET "/data/mwl" [] #'data/mwl-handler)
           (GET "/data/cycles" [] #'data/cycles-handler)
           (GET "/data/donors" [] #'data/donors-handler)

           (GET "/chat/config" [] #'chat/config-handler)
           (GET "/messages/:channel" [] #'chat/messages-handler)

           (GET "/reset/:token" [] #'pages/reset-password-page)

           (GET "/chsk" req (ws/handshake-handler req))
           (POST "/chsk" req (ws/post-handler req))

           (GET "/replay/:gameid" [] #'stats/replay-handler)
           (GET "/bug-report/:bugid" [] #'stats/replay-handler))

(defroutes missing-resource-routes
           (GET "/*/*" [] {:status 404 :body "Resource not found"}))

(defroutes public-CSRF-page-routes
           (GET "/*" [] #'pages/index-page))

(defroutes public-routes
           (POST "/register" [] #'auth/register-handler)
           (POST "/login" [] #'auth/login-handler)
           (POST "/forgot" [] #'auth/forgot-password-handler)
           (POST "/reset/:token" [] #'auth/reset-password-handler))

(defroutes api-routes
           (GET "/game/decklist" [] #'game-api/decklist-handler)
           (GET "/game/hand" [] #'game-api/hand-handler)
           (GET "/game/discard" [] #'game-api/discard-handler)
           (GET "/game/deck" [] #'game-api/deck-handler)
           (GET "/game/log" [] #'game-api/log-handler))

(defroutes admin-routes
           (POST "/admin/news" [] #'admin/news-create-handler)
           (DELETE "/admin/news/:id" [] #'admin/news-delete-handler)
           (GET "/admin/version" [] #'admin/version-handler)
           (PUT "/admin/version" [] #'admin/version-update-handler)
           (GET "/admin/features" [] #'admin/features-handler)
           (PUT "/admin/features" [] #'admin/features-update-handler))

(defroutes user-routes
           (POST "/logout" [] #'auth/logout-handler)
           (PUT "/profile" [] #'auth/update-profile-handler)
           (GET "/profile/email" [] #'auth/email-handler)
           (PUT "/profile/email" [] #'auth/change-email-handler)

           (DELETE "/profile/stats/user" [] #'stats/clear-userstats-handler)
           (DELETE "/profile/stats/deck/:id" [] #'stats/clear-deckstats-handler)

           (GET "/profile/history" [] #'stats/history)
           (GET "/profile/history/:gameid" [] #'stats/fetch-log)
           (GET "/profile/history/annotations/:gameid" [] #'stats/fetch-annotations)
           (PUT "/profile/history/annotations/publish/:gameid" [] #'stats/publish-annotations)
           (DELETE "/profile/history/annotations/delete/:gameid" [date] #'stats/delete-annotations)
           (GET "/profile/history/share/:gameid" [] #'stats/share-replay)
           (GET "/profile/history/full/:gameid" [] #'stats/fetch-replay)

           (GET "/data/decks" [] #'decks/decks-handler)
           (POST "/data/decks" [] #'decks/decks-create-handler)
           (PUT "/data/decks" [] #'decks/decks-save-handler)
           (DELETE "/data/decks/:id" [] #'decks/decks-delete-handler)

           (GET "/data/api-keys" [] #'api-keys/api-keys-handler)
           (POST "/data/api-keys" [] #'api-keys/api-keys-create-handler)
           (DELETE "/data/api-keys/:id" [] #'api-keys/api-keys-delete-handler))

(defroutes tournament-routes
  (GET "/tournament-auth/:username" [] #'tournament/auth))

(defroutes private-routes
  (wrap-routes user-routes auth/wrap-authentication-required)
  (wrap-routes tournament-routes auth/wrap-tournament-auth-required)
  (wrap-routes admin-routes auth/wrap-authorization-required))

(defroutes routes
  private-routes
  public-CSRF-routes
  public-routes
  (-> api-routes
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get]
                 :access-control-allow-headers #{"X-JNet-API"
                                                 "accept" "accept-encoding" "accept-language"
                                                 "authorization" "content-type" "origin"})
      (wrap-add-cache-headers))

  (route/resources "/")
  missing-resource-routes
  public-CSRF-page-routes
  (route/not-found "Page not found"))

(defn wrap-return-favicon [handler]
  (fn [req]
    (if (= [:get "/favicon.ico"] [(:request-method req) (:uri req)])
      (resource-response "jinteki.ico" {:root "public/img"})
      (handler req))))

(defn wrap-db [handler mongo]
  (fn [req]
    (handler (assoc req :system/db (:db mongo)))))

(defn make-app [mongo]
  (-> routes
      (auth/wrap-user)
      (wrap-db mongo)
      (wrap-json-response)
      (wrap-json-body {:keywords? true})
      (wrap-keyword-params)
      (wrap-params)
      (wrap-anti-forgery)
      (wrap-session)
      (wrap-return-favicon)
      (wrap-stacktrace)))
