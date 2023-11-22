(ns web.auth
  (:require
   [buddy.sign.jwt :as jwt]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [crypto.password.bcrypt :as password]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.result :refer [acknowledged?]]
   [postal.core :as mail]
   [ring.util.response :refer [redirect]]
   [web.app-state :as app-state]
   [web.mongodb :refer [find-one-as-map-case-insensitive ->object-id]]
   [web.user :refer [active-user? valid-username? within-char-limit-username? create-user user-keys]]
   [web.utils :refer [response md5]]
   [web.versions :refer [banned-msg]])
  (:import
   java.security.SecureRandom))

(defn create-token [{:keys [expiration secret]}
                    {:keys [_id emailhash]}]
  (let [claims {:_id _id
                :emailhash emailhash
                :exp (inst/plus (inst/now) expiration chrono/days)}]
    (jwt/sign claims secret {:alg :hs512})))

(defn unsign-token [{:keys [secret]} token]
  (try (jwt/unsign token secret {:alg :hs512})
       (catch Exception _ (prn "Received invalid cookie " token))))

(defn wrap-authentication-required [handler]
  (fn [{user :user :as req}]
    (if (active-user? user)
      (handler req)
      (response 401 {:message "Not authorized"}))))

(defn wrap-authorization-required [handler]
  (fn [{user :user :as req}]
    (if (:isadmin user)
      (handler req)
      (response 401 {:message "Not authorized"}))))

(defn wrap-tournament-auth-required [handler]
  (fn [{user :user :as req}]
    (if (:tournament-organizer user)
      (handler req)
      (response 401 {:message "Not authorized"}))))

(defn wrap-user [handler]
  (fn [{db :system/db
        auth :system/auth
        :keys [cookies] :as req}]
    (let [user (some-> (get cookies "session")
                       (:value)
                       (->> (unsign-token auth))
                       (#(mc/find-one-as-map db "users" {:_id (->object-id (:_id %))
                                                         :emailhash (:emailhash %)}))
                       (select-keys user-keys)
                       (update :_id str))]
      (if (active-user? user)
        (handler (-> req
                     (assoc :user user)
                     (assoc-in [:session :uid] (:username user))))
        (handler req)))))

(defn register-handler
  [{db :system/db
    {:keys [username password confirm-password email]} :params}]
  (cond
    (not (valid-username? username))
    (response 401 {:message "Username is not valid"})

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
  (active-user? (find-one-as-map-case-insensitive db "users" query)))

(defn login-handler
  [{db :system/db
    auth :system/auth
    {:keys [username password]} :params}]
  (let [user (mc/find-one-as-map db "users" {:username username})]
    (cond
      (and user (:banned user)) (response 403 {:error (or @banned-msg "Account Locked")})
      (and user (password/check password (:password user)))
      (do (mc/update db "users"
                     {:username username}
                     {"$set" {:lastConnection (inst/now)}})
          (assoc (response 200 {:message "ok"})
                 :cookies {"session" (merge {:value (create-token auth user)}
                                            (:cookie auth))}))
      :else (response 401 {:error "Invalid login or password"}))))

(defn logout-handler [_]
  (assoc (response 200 {:message "ok"})
         :cookies {"session" {:value 0
                              :max-age -1}}))

(defn check-username-handler
  [{db :system/db
    {:keys [username]} :path-params}]
  (if (find-one-as-map-case-insensitive db "users" {:username username})
    (response 422 {:message "Username taken"})
    (response 200 {:message "OK"})))

(defn check-email-handler
  [{db :system/db
    {:keys [email]} :path-params}]
  (if (find-one-as-map-case-insensitive db "users" {:email email})
    (response 422 {:message "Email taken"})
    (response 200 {:message "OK"})))

(defn email-handler
  [{db :system/db
    {username :username :as user} :user}]
  (if (active-user? user)
    (let [{:keys [email]} (find-one-as-map-case-insensitive db "users" {:username username})]
      (response 200 {:email email}))
    (response 401 {:message "Unauthorized"})))

(defn change-email-handler
  [{db :system/db
    {username :username :as user} :user
    {email :email} :body}]
  (cond
    (not (active-user? user))
    (response 401 {:message "Unauthorized"})

    (mc/find-one-as-map db "users" {:email email})
    (response 400 {:message "Email address already in use"})

    (acknowledged?
      (mc/update db "users"
                 {:username username}
                 {"$set" {:email email
                          :emailhash (md5 email)}}))
    (response 200 {:message "Refresh your browser"})

    :else
    (response 404 {:message "Account not found"})))

(defn profile-keys []
  [:background :pronouns :language :default-format :show-alt-art :blocked-users
   :alt-arts :card-resolution :deckstats :gamestats :card-zoom :pin-zoom
   :card-back :stacked-cards :sides-overlap :archives-sorted :heap-sorted
   :labeled-cards :labeled-unrezzed-cards])

(defn update-profile-handler
  [{db :system/db
    {username :username :as user} :user
    body :body}]
  (if (active-user? user)
    (if (acknowledged? (mc/update db "users"
                                  {:username username}
                                  {"$set" {:options (select-keys body (profile-keys))}}))
      (do (when (get-in @app-state/app-state [:users username])
            (swap! app-state/app-state assoc-in [:users username :options] (select-keys body (profile-keys))))
          (response 200 {:message "Refresh your browser"}))
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
        reset-expires (inst/plus (inst/now) 1 chrono/hours)]
    (mc/update db "users"
               {:email email}
               {"$set" {:resetPasswordToken reset-code
                        :resetPasswordExpires reset-expires}})
    reset-code))

(defn forgot-password-handler
  [{db :system/db
    email-settings :system/email
    {:keys [email]} :params
    headers         :headers}]
  (if-let [user (find-non-banned-user db {:email email})]
    (let [code (set-password-reset-code! db email)
          msg (mail/send-message
                email-settings
                {:from    "support@jinteki.net"
                 :to      email
                 :subject "Jinteki Password Reset"
                 :body    (str "You are receiving this because you (or someone else) have requested the reset of the password for your account " (user :username) ".\n\n"
                               "Please click on the following link, or paste this into your browser to complete the process:\n\n"
                               "http://" (headers "host") "/reset/" code "\n\n"
                               "If you did not request this, please ignore this email and your password will remain unchanged.\n")})]
      (if (zero? (:code msg))
        (response 200 {:message "Email sent"})
        (response 500 {:message (:message msg)})))
    (response 421 {:message "No account with that email address"})))

(defn reset-password-handler
  [{db :system/db
    email-settings :system/email
    {:keys [password confirm]} :params
    {:keys [token]} :path-params}]
  (if-let [{:keys [username email]}
           (find-non-banned-user db {:resetPasswordToken   token
                                     :resetPasswordExpires {"$gt" (inst/now)}})]
    (if (and password (= password confirm))
      (let [hash-pw (password/encrypt password)]
        (mc/update db "users"
                   {:username username}
                   {"$set" {:password             hash-pw
                            :resetPasswordExpires nil
                            :resetPasswordToken   nil}})
        (mail/send-message
          email-settings
          {:from    "support@jinteki.net"
           :to      email
           :subject "Your password has been changed"
           :body    (str "Hello,\n\n"
                         "This is a confirmation that the password for your account "
                         email " has just been changed.\n")})
        (redirect "/"))
      (response 422 {:message "New Password and Confirm Password did not match"}))
    (response 404 {:message "No reset token found"})))
