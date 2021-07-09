(ns web.auth
  (:require
    [clojure.string :as str]
    ;; external
    [buddy.sign.jwt :as jwt]
    [clj-time.coerce :as c]
    [clj-time.core :as t]
    [crypto.password.bcrypt :as password]
    [monger.collection :as mc]
    [monger.operators :refer :all]
    [monger.result :refer [acknowledged?]]
    [postal.core :as mail]
    [ring.util.response :refer [redirect]]
    ;; internal
    [web.config :refer [server-config]]
    [web.mongodb :refer [find-one-as-map-case-insensitive object-id]]
    [web.utils :refer [response md5]])
  (:import java.security.SecureRandom))

(defn active-user?
  [user]
  (and user (not (:banned user)) user))

(def auth-config (:auth server-config))

(defn create-token [{:keys [_id emailhash]}]
  (let [claims {:_id _id
                :emailhash emailhash
                :exp (-> (:expiration auth-config) (t/days) (t/from-now))}]
    (jwt/sign claims (:secret auth-config) {:alg :hs512})))

(defn unsign-token [token]
  (try (jwt/unsign token (:secret auth-config) {:alg :hs512})
       (catch Exception _ (prn "Received invalid cookie " token))))

(defn wrap-authentication-required [handler]
  (fn [{user :user :as req}]
    (if (active-user? user)
      (handler req)
      (response 401 {:message "Not authorized"}))))

(defn wrap-authorization-required [handler]
  (fn [{user :user :as req}]
    (if (and (active-user? user)
             (:isadmin user))
      (handler req)
      (response 401 {:message "Not authorized"}))))

(defn wrap-tournament-auth-required [handler]
  (fn [{user :user :as req}]
    (if (and (active-user? user)
             (:tournament-organizer user))
      (handler req)
      (response 401 {:message "Not authorized"}))))

(def user-keys
  [:_id :username :emailhash
   :isadmin :ismoderator :tournament-organizer
   :special :options :stats :has-api-keys :banned])

(defn wrap-user [handler]
  (fn [{db :system/db
        :keys [cookies] :as req}]
    (let [auth-cookie (get cookies "session")
          user (when auth-cookie
                 (unsign-token (:value auth-cookie)))
          u (when user
              (mc/find-one-as-map db "users" {:_id (object-id (:_id user))
                                              :emailhash (:emailhash user)}))]
      (if (active-user? u)
        (handler (-> req
                     (assoc :user (select-keys u user-keys))
                     (update-in [:user :_id] str)))
        (handler req)))))

(defn create-user
  "Create a new user map."
  [username password email & {:keys [isadmin]}]
  (let [email-hash (md5 email)
        registration-date (java.util.Date.)
        last-connection registration-date
        hash-pw (password/encrypt password)
        isadmin (or isadmin false)]
    {:username         username
     :email            email
     :emailhash        email-hash
     :registrationDate registration-date
     :lastConnection   last-connection
     :password         hash-pw
     :isadmin          isadmin
     :options          {}}))

(defn register-handler
  [{db :system/db
    {:keys [username password confirm-password email]} :params}]
  (cond
    (< 20 (count username))
    (response 401 {:message "Usernames are limited to 20 characters"})

    (not= password confirm-password)
    (response 401 {:message "Passwords must match"})

    (find-one-as-map-case-insensitive db "users" {:username username})
    (response 422 {:message "Username taken"})

    (find-one-as-map-case-insensitive db "users" {:email email})
    (response 424 {:message "Email taken"})

    :else
    (let [first-user (not (mc/any? db "users"))
          demo-decks (mc/find-maps db "decks" {:username "__demo__"})]
      (mc/insert db "users" (create-user username password email :isadmin first-user))
      (when (not-empty demo-decks)
        (mc/insert-batch db "decks" (map #(-> %
                                              (dissoc :_id)
                                              (assoc :username username))
                                         demo-decks)))
      (response 200 {:message "ok"}))))

(defn find-non-banned-user
  [db query]
  (when-let [user (mc/find-one-as-map db "users" query)]
    (active-user? user)))

(defn login-handler
  [{db :system/db
    {:keys [username password]} :params}]
  (let [user (find-non-banned-user db {:username username})]
    (if (and user
             (password/check password (:password user)))

      (do (mc/update db "users"
                     {:username username}
                     {"$set" {:last-connection (java.util.Date.)}})
          (assoc (response 200 {:message "ok"})
                 :cookies {"session" (merge {:value (create-token user)}
                                            (get-in server-config [:auth :cookie]))}))
      (response 401 {:error "Invalid login or password"}))))

(defn logout-handler [_]
  (assoc (response 200 {:message "ok"})
         :cookies {"session" {:value 0
                              :max-age -1}}))

(defn check-username-handler
  [{db :system/db
    {:keys [username]} :params}]
  (if (find-one-as-map-case-insensitive db "users" {:username username})
    (response 422 {:message "Username taken"})
    (response 200 {:message "OK"})))

(defn check-email-handler
  [{db :system/db
    {:keys [email]} :params}]
  (if (find-one-as-map-case-insensitive db "users" {:email email})
    (response 422 {:message "Username taken"})
    (response 200 {:message "OK"})))

(defn email-handler
  [{db :system/db
    {username :username} :user}]
  (if username
    (let [{:keys [email]} (find-one-as-map-case-insensitive db "users" {:username username})]
      (response 200 {:email email}))
    (response 401 {:message "Unauthorized"})))

(defn change-email-handler
  [{db :system/db
    {username :username} :user
    {email :email} :body}]
  (cond
    (not username)
    (response 401 {:message "Unauthorized"})

    (mc/find-one-as-map db "users" {:email email})
    (response 400 {:message "Email address already in use"})

    (acknowledged?
      (mc/update db "users"
                 {:username username}
                 {"$set" {:email email}}))
    (response 200 {:message "Refresh your browser"})

    :else
    (response 404 {:message "Account not found"})))

(defn profile-keys []
  [:background :pronouns :language :show-alt-art :blocked-users
   :alt-arts :card-resolution :deckstats :gamestats :card-zoom
   :pin-zoom :card-back :stacked-cards :sides-overlap])

(defn update-profile-handler
  [{db :system/db
    {username :username} :user
    body                 :body}]
  (if username
    (if (acknowledged? (mc/update db "users"
                                  {:username username}
                                  {"$set" {:options (select-keys body (profile-keys))}}))
      (response 200 {:message "Refresh your browser"})
      (response 404 {:message "Account not found"}))
    (response 401 {:message "Unauthorized"})))

(defn generate-secure-token
  [size]
  (let [seed (byte-array size)]
    (.nextBytes (SecureRandom/getInstance "SHA1PRNG") seed)
    seed))

(defn hexadecimalize
  "Converts a byte array to a hex string"
  [a-byte-array]
  (str/lower-case (str/join (map #(format "%02X" %) a-byte-array))))

(defn set-password-reset-code!
  "Generates a password-reset code for the given email address. Updates the user's info in the database with the code,
  and returns the code."
  [db email]
  (let [reset-code (hexadecimalize (generate-secure-token 20))
        reset-expires (t/plus (t/now) (t/hours 1))]
    (mc/update db "users"
               {:email email}
               {"$set" {:resetPasswordToken reset-code
                        :resetPasswordExpires (c/to-date reset-expires)}})
    reset-code))

(defn forgot-password-handler
  [{db :system/db
    {:keys [email]} :params
    headers         :headers}]
  (if (find-non-banned-user {:email email})
    (let [code (set-password-reset-code! db email)
          msg (mail/send-message
                (:email server-config)
                {:from    "support@jinteki.net"
                 :to      email
                 :subject "Jinteki Password Reset"
                 :body    (str "You are receiving this because you (or someone else) have requested the reset of the password for your account.\n\n"
                               "Please click on the following link, or paste this into your browser to complete the process:\n\n"
                               "http://" (headers "host") "/reset/" code "\n\n"
                               "If you did not request this, please ignore this email and your password will remain unchanged.\n")})]
      (if (zero? (:code msg))
        (response 200 {:message "Email sent"})
        (response 500 {:message (:message msg)})))
    (response 421 {:message "No account with that email address"})))

(defn reset-password-handler
  [{db :system/db
    {:keys [password confirm token]} :params}]
  (if-let [{:keys [username email]}
           (find-non-banned-user {:resetPasswordToken   token
                                  :resetPasswordExpires {"$gt" (c/to-date (t/now))}})]
    (if (= password confirm)
      (let [hash-pw (password/encrypt password)]
        (mc/update db "users"
                   {:username username}
                   {"$set" {:password             hash-pw
                            :resetPasswordExpires nil
                            :resetPasswordToken   nil}})
        (mail/send-message
          (:email server-config)
          {:from    "support@jinteki.net"
           :to      email
           :subject "Your password has been changed"
           :body    (str "Hello,\n\n"
                         "This is a confirmation that the password for your account "
                         email " has just been changed.\n")})
        (redirect "/"))
      (response 422 {:message "New Password and Confirm Password did not match"}))
    (response 404 {:message "No reset token found"})))
