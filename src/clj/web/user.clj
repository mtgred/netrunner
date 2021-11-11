(ns web.user
  (:require
   [crypto.password.bcrypt :as password]
   [web.utils :refer [md5]]
   [clojure.set :as set]))

(def user-keys
  [:_id :username :emailhash
   :isadmin :ismoderator :tournament-organizer
   :special :options :stats :has-api-keys :banned])

(defn create-user
  "Create a new user map."
  [username password email & {:keys [isadmin]}]
  (let [registration-date (java.util.Date.)]
    {:username         username
     :email            email
     :emailhash        (md5 email)
     :registrationDate registration-date
     :lastConnection   registration-date
     :password         (password/encrypt password)
     :isadmin          (or isadmin false)
     :options          {}}))

(defn active-user?
  "Returns the given user if it exists and is not banned"
  [user]
  (when (and user (not (:banned user)))
    user))

(defn visible-users
  "Given a user and a list of username strings,
  return a seq of the usernames not blocked by the user"
  [user users]
  (let [blocked (set (-> user :options :blocked-users))
        others (set users)]
    (seq (set/difference others blocked))))
