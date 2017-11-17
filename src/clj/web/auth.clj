(ns web.auth
  (:require [web.db :refer [db object-id]]
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

(defn create-token [{:keys [_id emailhash]}]
  (let [claims {:_id _id
                :emailhash emailhash
                :exp (-> (:expiration auth-config) days from-now)}
        token (jwt/sign claims (:secret auth-config) {:alg :hs512})]
    (jwt/sign claims (:secret auth-config) {:alg :hs512})))

(defn unsign-token [token]
  (try (jwt/unsign token (:secret auth-config) {:alg :hs512})
       (catch Exception e (prn "Received invalid cookie " token))))

(defn wrap-user [handler]
  (fn [{:keys [cookies] :as req}]
    (let [auth-cookie (get cookies "session")
          {:keys [_id emailhash] :as user} (when auth-cookie (unsign-token (:value auth-cookie)))
          u (when user (mc/find-one-as-map db "users" {:_id (object-id _id) :emailhash emailhash}))]
      (if u
        (handler (-> req
                     (assoc :user (select-keys u [:_id :username :emailhash :isadmin :special :options]))
                     (update-in [:user :_id] str)))
        (handler req)))))

(defn login-handler [{{:keys [username password]} :params
                      :as request}]
  (let [user (mc/find-one-as-map db "users" {:username username})]
    (if (and user
             (password/check password (:password user)))

      (do (mc/update db "users"
                     {:username username}
                     {"$set" {:last-connection (java.util.Date.)}})
          (-> (response 200 {:message "ok"})
              (assoc :cookies {"session" {:value (create-token user)
                                          :http-only true}})))
      (response 401 {:error "Invalid login or password"}))))

(defn logout-handler [request]
  (assoc (response 200 {:message "ok"})
    :cookies {"session" {:value 0
                         :max-age -1}}))

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
