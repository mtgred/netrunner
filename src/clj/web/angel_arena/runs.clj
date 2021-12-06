(ns web.angel-arena.runs
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :refer [lower-case]]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [web.angel-arena.utils :refer [get-runs]]
   [web.mongodb :refer [->object-id]]))

(defn start-run!
  [db username runs deck]
  (let [form (keyword (lower-case (get-in deck [:status :format])))
        side (keyword (lower-case (get-in deck [:identity :side])))
        deck-id (str (:_id deck))]
    (when (get-in deck [:status form :legal]) ; deck is legal in this format
      ;add run to user account
      (mc/update db "users"
                 {:username username}
                 {"$set" {:angel-arena-runs
                          (assoc-in runs [form side]
                                    {:deck-id deck-id
                                     :format form
                                     :side side
                                     :games []
                                     :run-started (inst/now)})}})
      ; lock deck
      (mc/update db "decks"
                 {:_id (->object-id deck-id)
                  :username username}
                 {"$set" {:locked true}}))))

(defn finish-run!
  [db username runs deck]
  (let [form (keyword (lower-case (get-in deck [:status :format])))
        side (keyword (lower-case (get-in deck [:identity :side])))
        deck-id (str (:_id deck))]
    ; remove run from user account
    (mc/update db "users"
               {:username username}
               {"$set" {:angel-arena-runs
                        (assoc-in runs [form side] nil)}})
    ; unlock deck
    (mc/update db "decks"
               {:_id (->object-id deck-id)
                :username username}
               {"$set" {:locked false}})
    ; add run to run history
    (when-not (empty? (get-in runs [form side :games]))
      (mc/insert db "angel-arena"
                 (assoc (get-in runs [form side])
                        :identity (get-in deck [:identity :title])
                        :deck-name (:name deck)
                        :run-finished (inst/now)
                        :username username)))))

(defn add-new-match!
  [db player other-player game-id]
  (try
    (let [username (get-in player [:user :username])
          runs (get-runs db username)
          side (keyword (lower-case (:side player)))
          form (:format player)
          other-username (get-in other-player [:user :username])
          other-pronouns (get-in other-player [:user :options :pronouns])
          other-identity (get-in other-player [:deck :identity :title])]
      (mc/update db "users"
                 {:username username}
                 {"$set" {:angel-arena-runs
                          (update-in runs [form side :games]
                                     conj {:game-id game-id
                                           :winner nil
                                           :reason nil
                                           :opponent {:username other-username
                                                      :pronouns other-pronouns
                                                      :identity other-identity}})}}))
    (catch Exception e
      (println "Caught exception adding new game to Angel Arena history: " (.getMessage e)))))
