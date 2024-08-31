(ns web.user
  (:require
   [cljc.java-time.instant :as inst]
   [crypto.password.bcrypt :as password]
   [web.utils :refer [md5]]))

(def user-keys
  [:_id :username :emailhash
   :isadmin :ismoderator :tournament-organizer
   :special :options :stats :has-api-keys :banned])

(defn character-length [^String username] (.codePointCount username 0 (count username)))

(def url-invalid-char-pattern      #"://")
(def end-html-invalid-char-pattern #"</")
(def patterns-username
  [url-invalid-char-pattern
  end-html-invalid-char-pattern])

(defn within-char-limit-username? [username] (<= (character-length username) 20))

(defn valid-username?
  "Validate a username"
  [username]
  (and (within-char-limit-username? username)
       (not (some #(re-find % username) patterns-username))))

(defn create-user
  "Create a new user map."
  [username password email & {:keys [isadmin]}]
  (let [registration-date (inst/now)]
    {:username         username
     :email            email
     :emailhash        (md5 email)
     :registrationDate registration-date
     :lastConnection   registration-date
     :password         (password/encrypt password)
     :isadmin          (or isadmin false)
     :options          {:default-format "standard"
                        :pronouns "none"}}))

(defn active-user?
  "Returns the given user if it exists and is not banned"
  [user]
  (when (and user (not (:banned user)))
    user))

(defn visible-to-user
  "Returns true if user has not blocked other and other has not blocked user"
  [user other connected-users]
  (let [user-block-list (-> user :options :blocked-users (set))
        other-username (:username other)
        other-block-list (-> (get connected-users other-username)
                             :options
                             :blocked-users
                             (set))]
    (not (or (contains? user-block-list other-username)
             (contains? other-block-list (:username user))))))
