(ns nr.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [nr.ajax :refer [GET]]
            [nr.ws :as ws]
            [nr.appstate :refer [app-state]]
            ;[nr.auth :refer [authenticated avatar] :as auth]
            ;[nr.gameboard :refer [init-game game-state toast launch-game parse-state]]
            ;[nr.cardbrowser :refer [image-url non-game-toast] :as cb]
            ;[nr.stats :refer [notnum->zero]]
            ;[nr.deckbuilder :refer [format-deck-status-span deck-status-span process-decks load-decks num->percent]]
            [taoensso.sente  :as sente]))


(defn- blocked-from-game
  "Remove games for which the user is blocked by one of the players"
  [username game]
  (let [players (get game :players [])
        blocked-users (flatten (map #(get-in % [:user :options :blocked-users] []) players))]
    (= -1 (.indexOf blocked-users username))))

(defn- blocking-from-game
  "Remove games with players we are blocking"
  [blocked-users game]
  (let [players (get game :players [])
        player-names (map #(get-in % [:user :username] "") players)
        intersect (clojure.set/intersection (set blocked-users) (set player-names))]
    (empty? intersect)))

(defn filter-blocked-games
  [user games]
  (let [blocked-games (filter #(blocked-from-game (:username user) %) games)
        blocked-users (get-in user [:options :blocked-users] [])]
    (filter #(blocking-from-game blocked-users %) blocked-games)))