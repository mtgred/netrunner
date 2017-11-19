(ns web.api
  (:require [web.utils :refer [response]]
            [web.data :as data]
            [web.pages :as pages]
            [web.auth :as auth]
            [web.ws :as ws]
            [web.game :as game]
            [web.chat :as chat]
            [web.stats :as stats]
            [immutant.web]
            [web.admin :as admin]
            [cheshire.core :refer [generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [compojure.core :refer [defroutes GET POST DELETE PUT]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-params wrap-json-response]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [hiccup.page :as hiccup]
            [web.db :refer [db]]
            [monger.collection :as mc]))

(add-encoder org.bson.types.ObjectId encode-str)

(defroutes public-routes
           (route/resources "/")
           (POST "/register" [] auth/register-handler)
           (POST "/login" [] auth/login-handler)
           (POST "/logout" [] auth/logout-handler)
           (GET "/check/:username" [] auth/check-username-handler)
           (PUT "/profile" [] auth/update-profile-handler)

           (POST "/user/clearstats" [] stats/clear-user-stats)
           (POST "/data/decks/clearstats" [] stats/clear-deck-stats)

           (GET "/data/cards" [] data/cards-handler)
           (GET "/data/altarts" [] data/alt-arts-handler)

           (GET "/data/decks" [] data/decks-handler)
           (POST "/data/decks" [] data/decks-create-handler)
           (PUT "/data/decks" [] data/decks-save-handler)
           (DELETE "/data/decks/:id" [] data/decks-delete-handler)


           (GET "/data/news" [] data/news-handler)
           (GET "/data/sets" [] data/sets-handler)
           (GET "/data/mwl" [] data/mwl-handler)
           (GET "/data/cycles" [] data/cycles-handler)
           (GET "/data/donors" [] data/donors-handler)

           (GET "/messages/:channel" [] chat/messages-handler)

           (GET "/ws" req ws/handshake-handler)
           (POST "/ws" req ws/post-handler)

           (GET "/" [] pages/index-page)
           )

(defroutes admin-routes
           (GET "/admin/announce" [] pages/announce-page)
           (POST "/admin/announce" [] admin/announcement-handler)
           (GET "/admin/version" [] pages/version-page)
           (POST "/admin/version" [] admin/version-handler))

(defroutes routes

           (-> public-routes
               auth/wrap-user)
           (-> admin-routes
               auth/wrap-authorization
               auth/wrap-user)
           )

(def app
  (-> routes
      wrap-keyword-params
      wrap-params
      wrap-json-response
      wrap-session
      (wrap-json-body {:keywords? true})
      admin/wrap-version
      wrap-stacktrace
      ))