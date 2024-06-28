(ns web.api
  (:require
   [cheshire.generate :refer [add-encoder encode-str]]
   [reitit.core :as r]
   [reitit.ring :as ring]
   [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.content-type :refer [wrap-content-type]]
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

(def paths ["" "cards" "deckbuilder" "play" "help" "account" "stats" "about" "tournament" "admin" "users" "features"])

(defn base-routes []
  (ring/router
    (mapv (fn [path] [(str "/" path) {:get pages/index-page :middleware [wrap-anti-forgery]}]) paths)))

(comment
  ((ring/ring-handler (base-routes)) {:request-method :get :uri "/"})
  )

;; Taken from puppetlabs/ring-middleware
(defn wrap-add-cache-headers
  "Adds cache control invalidation headers to GET and PUT requests if they are handled by the handler"
  [handler]
  (fn [request]
    (let [request-method (:request-method request)
          response       (handler request)]
      (when-not (nil? response)
        (if (or
             (= request-method :get)
             (= request-method :put))
          (assoc-in response [:headers "cache-control"] "no-store")
          response)))))

(defn api-routes []
  (ring/router
    [["/chsk" {:get ws/handshake-handler
               :post ws/post-handler
               :middleware [::forgery]}]
     ["/data" {:middleware [::forgery]}
      ["/cards"
       ["" {:get data/cards-handler}]
       ["/version" {:get data/cards-version-handler}]
       ["/altarts" {:get data/alt-arts-handler}]
       ["/lang/:lang" {:get data/lang-handler}]]
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
       ["/:id" {:delete api-keys/api-keys-delete-handler}]]]
     ["/chat/config" {:get chat/config-handler :middleware [::forgery]}]
     ["/messages/:channel" {:get chat/messages-handler :middleware [::forgery]}]
     ["/reset/:token" {:get pages/reset-password-page
                       :post auth/reset-password-handler}]
     ["/replay/:gameid" {:get stats/replay-handler :middleware [::forgery]}]
     ["/bug-report/:bugid" {:get stats/replay-handler :middleware [::forgery]}]
     ["/register" {:post auth/register-handler :middleware [::forgery]}]
     ["/check-username/:username" {:get auth/check-username-handler :middleware [::forgery]}]
     ["/check-email/:email" {:get auth/check-email-handler :middleware [::forgery]}]
     ["/login" {:post auth/login-handler :middleware [::forgery]}]
     ["/forgot" {:post auth/forgot-password-handler :middleware [::forgery]}]
     ["/logout" {:middleware [::auth ::forgery]
                 :post auth/logout-handler}]
     ["/game" {:middleware [::forgery
                            ::cors
                            wrap-add-cache-headers]}
      ["/decklist" {:get game-api/decklist-handler}]
      ["/hand" {:get game-api/hand-handler}]
      ["/discard" {:get game-api/discard-handler}]
      ["/deck" {:get game-api/deck-handler}]
      ["/log" {:get game-api/log-handler}]]
     ["/profile" {:middleware [::auth ::forgery]}
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
     ["/tournament-auth/:username" {:middleware [::auth ::tournament-auth ::forgery]
                                    :get tournament/auth}]
     ["/admin" {:middleware [::auth ::admin ::forgery]}
      ["/news"
       ["" {:post admin/news-create-handler}]
       ["/:id" {:delete admin/news-delete-handler}]]
      ["/version" {:get admin/version-handler
                   :put admin/version-update-handler}]
      ["/banned" {:get admin/banned-message-handler
                   :put admin/banned-message-update-handler}]
      ["/features" {:get admin/features-handler
                    :put admin/features-update-handler}]]]
    {:reitit.middleware/registry
     {::auth auth/wrap-authentication-required
      ::tournament-auth auth/wrap-tournament-auth-required
      ::admin auth/wrap-authorization-required
      ::forgery wrap-anti-forgery
      ::cors [[wrap-cors
               :access-control-allow-origin [#".*"]
               :access-control-allow-methods [:get]
               :access-control-allow-headers #{"X-JNet-API"
                                               "accept" "accept-encoding" "accept-language"
                                               "authorization" "content-type" "origin"}]]}}))

(comment
  ((ring/ring-handler (api-routes)) {:request-method :get :uri "/data/cards"})
  )

(defn merge-routes [& routes]
  (ring/router
    (apply merge (map r/routes routes))
    (apply merge (map r/options routes))))

(comment
  (r/routes (merge-routes (base-routes) (api-routes)))
  ((ring/ring-handler (merge-routes (base-routes) (api-routes))) {:request-method :get :uri "/play"})
  )

(defn make-default-routes []
  (ring/routes
    (ring/redirect-trailing-slash-handler)
    (ring/create-resource-handler {:path "/"})
    ; (ring/create-default-handler
    ;   {:not-found pages/index-page})
    ))

(defn wrap-return-favicon [handler]
  (fn [request]
    (if (= [:get "/favicon.ico"] [(:request-method request) (:uri request)])
      (resource-response "jinteki.ico" {:root "public/img"})
      (handler request))))

(def wrap-system
  (middleware/map->Middleware
    {:name ::wrap-system
     :description "Adds the relevant integrant system pieces to requests"
     :wrap (fn [handler system]
             (fn [request]
               (-> request
                   (assoc :system/db (-> system :mongodb/connection :db))
                   (assoc :system/server-mode (:server-mode system))
                   (assoc :system/auth (:web/auth system))
                   (assoc :system/chat (:web/chat system))
                   (assoc :system/email (:web/email system))
                   (handler))))}))

(defn make-middleware [system]
  {:middleware [wrap-return-favicon
                wrap-session
                wrap-content-type
                ;; Removed to allow reset password flow to work
                ;; wrap-anti-forgery
                wrap-params
                wrap-keyword-params
                [wrap-json-body {:keywords? true}]
                wrap-json-response
                [wrap-system system]
                auth/wrap-user]})

(defn make-app [system]
  (ring/ring-handler
    (merge-routes (base-routes) (api-routes))
    (make-default-routes)
    (make-middleware system)))

(defn make-dev-app [system]
  (ring/ring-handler
    (merge-routes)
    (make-default-routes)
    (update (make-middleware system) :middleware #(vec (cons wrap-stacktrace %)))))

(comment
  ((make-app nil) {:request-method :get :uri "/chat/config"})
  (r/router-name (ring/get-router (make-app nil))))
