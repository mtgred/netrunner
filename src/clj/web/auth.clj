(ns web.auth
  (:require [web.db :refer [db]]
            [web.utils :refer [response]]
            [aero.core :refer [read-config]]
            [clj-time.core :refer [days from-now]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [buddy.sign.jwt :as jwt]
            [buddy.auth :refer [authenticated?]]
            [buddy.auth.backends.session :refer [session-backend]]
            [crypto.password.bcrypt :as password]))

(def auth-config (:auth (read-config "dev.edn")))

(def backend (session-backend))
               ;{:secret (:secret auth-config)
               ;            :options {:alg :hs512}}))

(defn create-token [{:keys [username emailhash]}]
  (let [claims {:username username
                :emailhash emailhash
                :exp (-> (:expiration auth-config) days from-now)}
        token (jwt/sign claims (:secret auth-config) {:alg :hs512})]
    (jwt/sign claims (:secret auth-config) {:alg :hs512})))

(defn login-handler [{{:keys [username password]} :params
                      session :session :as request}]
  (let [user (mc/find-one-as-map db "users" {:username username})]
    (if (and user
             (password/check password (:password user)))

      (do (mc/update db "users"
                     {:username username}
                     {"$set" {:last-connection (java.util.Date.)}})
          (assoc (response 200 {:user  (select-keys user [:username :emailhash :isadmin :options])
                                :token (create-token user)})
            :session (assoc session :identity (:_id user))))
      (response 401 {:error "Invalid login or password"}))))

(defn check-username-handler [{{:keys [username]} :params}]
  (if (mc/find-one-as-map db "users" {:username username})
    (response 422 {:message "Username taken"})
    (response 200 {:message "OK"})))

(defn update-profile-handler [{{username :username} :user
                               body                 :body}]
  (if username
    (if (acknowledged? (mc/update db "users"
                                  {:username username}
                                  {"$set" {:options (select-keys body [:background :show-alt-art :blocked-users :alt-arts
                                                                       :deckstats :gamestats])}}))
      (response 200 {:message "Refresh your browser"})
      (response 404 {:message "Account not found"}))
    (response 401 {:message "Unauthorized"})))

(defn is-authenticated? [{user :user :as req}]
  (not (nil? user)))

(defn wrap-user [handler]
  (fn [{user-id :identity :as req}]
    (if-let [u (mc/find-one-as-map db "users" {:_id user-id})]
      (handler (-> req
                   (assoc :user (when u
                                  (select-keys (mc/find-one-as-map db "users" {:_id user-id})
                                               [:_id :username :emailhash :isadmin :special :options])))
                   (update-in [:user :_id] str)))
      (handler req))))