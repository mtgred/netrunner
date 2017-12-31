(ns web.api
  (:require [jinteki.nav :as nav]
            [web.utils :refer [response]]
            [web.data :as data]
            [web.pages :as pages]
            [web.auth :as auth]
            [web.ws :as ws]
            [web.game :as game]
            [web.chat :as chat]
            [web.stats :as stats]
            [web.admin :as admin]
            [web.news :as news]
            [web.decks :as decks]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-params wrap-json-response]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [web.db :refer [db]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [compojure.core :refer [defroutes wrap-routes GET POST DELETE PUT]]            ))

(add-encoder org.bson.types.ObjectId encode-str)

(defroutes public-routes
           (route/resources "/")
           (POST "/register" [] auth/register-handler)
           (POST "/login" [] auth/login-handler)
           (GET "/check/:username" [] auth/check-username-handler)

           (GET "/data/cards" [] data/cards-handler)
           (GET "/data/cards/version" [] data/cards-version-handler)
           (GET "/data/cards/altarts" [] data/alt-arts-handler)

           (GET "/data/news" [] news/news-handler)
           (GET "/data/sets" [] data/sets-handler)
           (GET "/data/mwl" [] data/mwl-handler)
           (GET "/data/cycles" [] data/cycles-handler)
           (GET "/data/donors" [] data/donors-handler)

           (GET "/messages/:channel" [] chat/messages-handler)

           (POST "/forgot" [] auth/forgot-password-handler)
           (GET "/reset/:token" [] pages/reset-password-page)
           (POST "/reset/:token" [] auth/reset-password-handler)

           (GET "/ws" req ws/handshake-handler)
           (POST "/ws" req ws/post-handler)

           (GET "/*" [] pages/index-page))

(defroutes admin-routes
           (GET "/admin/announce" [] pages/announce-page)
           (POST "/admin/announce" [] admin/announcement-handler)
           (GET "/admin/version" [] pages/version-page)
           (POST "/admin/version" [] admin/version-handler))

(defroutes user-routes
           (POST "/logout" [] auth/logout-handler)
           (PUT "/profile" [] auth/update-profile-handler)

           (DELETE "/profile/stats/user" [] stats/clear-userstats-handler)
           (DELETE "/profile/stats/deck/:id" [] stats/clear-deckstats-handler)

           (GET "/data/decks" [] decks/decks-handler)
           (POST "/data/decks" [] decks/decks-create-handler)
           (PUT "/data/decks" [] decks/decks-save-handler)
           (DELETE "/data/decks/:id" [] decks/decks-delete-handler))

(defroutes routes
           (-> user-routes
               (wrap-routes auth/wrap-authentication-required))
           (-> admin-routes
               (wrap-routes auth/wrap-authorization-required))
           public-routes)

(def app
  (-> routes
      auth/wrap-user
      wrap-keyword-params
      wrap-params
      wrap-json-response
      wrap-session
      (wrap-json-body {:keywords? true})
      admin/wrap-version
      wrap-stacktrace))
