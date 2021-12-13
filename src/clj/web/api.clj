(ns web.api
  (:require
   [cheshire.generate :refer [add-encoder encode-str]]
   [puppetlabs.ring-middleware.core :refer [wrap-add-cache-headers]]
   [reitit.ring :as ring]
   [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.stacktrace :refer [wrap-stacktrace]]
   [ring.util.response :refer [resource-response]]
   [reitit.middleware :as middleware]
   [web.admin :as admin]
   [web.api-keys :as api-keys]
   [web.auth :as auth]
   [web.chat :as chat]
   [web.data :as data]
   [web.decks :as decks]
   [web.game-api :as game-api]
   [web.pages :as pages]
   [web.stats :as stats]
   [web.tournament :as tournament]
   [web.ws :as ws]))

(add-encoder org.bson.types.ObjectId encode-str)

(defn wrap-return-favicon [handler]
  (fn [request]
    (if (= [:get "/favicon.ico"] [(:request-method request) (:uri request)])
      (resource-response "jinteki.ico" {:root "public/img"})
      (handler request))))

(def wrap-db
  (middleware/map->Middleware
    {:name ::wrap-db
     :description "Adds the database connection to :system/db"
     :wrap (fn [handler mongo]
             (fn [request]
               (handler (assoc request :system/db (:db mongo)))))}))

(defn api-routes []
  (ring/router
    [["/chsk" {:get ws/handshake-handler
               :post ws/post-handler}]
     ["/data"
      ["/cards"
       ["" {:get data/cards-handler}]
       ["/version" {:get data/cards-version-handler}]
       ["/altarts" {:get data/alt-arts-handler}]]
      ["/news" {:get data/news-handler}]
      ["/sets" {:get data/sets-handler}]
      ["/mwl" {:get data/mwl-handler}]
      ["/cycles" {:get data/cycles-handler}]
      ["/donors" {:get data/donors-handler}]
      ["/decks"
       ["" {:get decks/decks-handler
            :post decks/decks-create-handler
            :put decks/decks-save-handler}]
       ["/:id" {:delete decks/decks-delete-handler}]]
      ["/api-keys" {:middleware [::auth]}
       ["" {:get api-keys/api-keys-handler
            :post api-keys/api-keys-create-handler}]
       ["/:id" {:get api-keys/api-keys-delete-handler}]]]
     ["/chat/config" {:get chat/config-handler}]
     ["/messages/:channel" {:get chat/messages-handler}]
     ["/reset/:token" {:get pages/reset-password-page
                       :post auth/reset-password-handler}]
     ["/replay/:gameid" {:get stats/replay-handler}]
     ["/bug-report/:bugid" {:get stats/replay-handler}]
     ["/register" {:post auth/register-handler}]
     ["/check-username/:username" {:get auth/check-username-handler}]
     ["/check-email/:email" {:get auth/check-email-handler}]
     ["/login" {:post auth/login-handler}]
     ["/forgot" {:post auth/forgot-password-handler}]
     ["/logout" {:middleware [::auth]
                 :post auth/logout-handler}]
     ["/game" {:middleware [::cors
                            wrap-add-cache-headers]}
      ["/decklist" {:get game-api/decklist-handler}]
      ["/hand" {:get game-api/hand-handler}]
      ["/discard" {:get game-api/discard-handler}]
      ["/deck" {:get game-api/deck-handler}]
      ["/log" {:get game-api/log-handler}]]
     ["/profile" {:middleware [::auth]}
      ["" {:put auth/update-profile-handler}]
      ["/email" {:get auth/email-handler
                 :put auth/change-email-handler}]
      ["/stats"
       ["/user" {:delete stats/clear-userstats-handler}]
       ["/deck/{id}" {:delete stats/clear-deckstats-handler}]]
      ["/history"
       ["" {:get stats/history}]
       ["/:gameid" {:get stats/fetch-log}]
       ["/annotations"
        ["/:gameid" {:get stats/fetch-annotations}]
        ["/publish/:gameid" {:get stats/publish-annotations}]
        ["/delete/:gameid" {:delete stats/delete-annotations}]]
       ["/share/:gameid" {:get stats/share-replay}]
       ["/full/:gameid" {:get stats/fetch-replay}]]]
     ["/tournament-auth/:username" {:middleware [::auth ::tournament-auth]
                                    :get tournament/auth}]
     ["/admin" {:middleware [::auth ::admin]}
      ["/news"
       ["" {:post admin/news-create-handler}]
       ["/:id" {:delete admin/news-delete-handler}]]
      ["/version" {:get admin/version-handler
                   :put admin/version-update-handler}]
      ["/features" {:get admin/features-handler
                    :put admin/features-update-handler}]]]
    {:reitit.middleware/registry
     {::auth auth/wrap-authentication-required
      ::tournament-auth auth/wrap-tournament-auth-required
      ::admin auth/wrap-authorization-required
      ::cors [[wrap-cors
               :access-control-allow-origin [#".*"]
               :access-control-allow-methods [:get]
               :access-control-allow-headers #{"X-JNet-API"
                                               "accept" "accept-encoding" "accept-language"
                                               "authorization" "content-type" "origin"}]]}}))

(defn make-app [mongo]
  (ring/ring-handler
    (api-routes)
    (ring/routes
      (ring/redirect-trailing-slash-handler)
      (ring/create-resource-handler {:path "/"})
      (ring/create-default-handler
        {:not-found pages/index-page}))
    {:middleware [wrap-stacktrace
                  wrap-return-favicon
                  wrap-session
                  wrap-anti-forgery
                  wrap-params
                  wrap-keyword-params
                  [wrap-json-body {:keywords? true}]
                  wrap-json-response
                  [wrap-db mongo]
                  auth/wrap-user]}))

(comment
  ((make-app nil) {:request-method :get :uri "/chat/config"})
  (require '[reitit.core :as r])
  (r/router-name (ring/get-router (make-app nil)))
  )
