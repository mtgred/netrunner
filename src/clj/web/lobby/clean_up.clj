(ns web.lobby.clean-up
  (:require
   [cljc.java-time.instant :as inst]
   [executor.core :as executor]))

(defn find-inactive-lobbies
  [lobbies now time-inactive]
  (->> (vals lobbies)
       (filter
         (fn [lobby]
           (inst/is-after now (inst/plus-seconds (:last-update lobby) time-inactive))))
       (seq)))

(defn handle-lobby-clean-up
  [{{lobbies :lobbies :as db} :db now :inst/now}
   {system-db :system/db time-inactive :time-inactive}]
  (when-let [inactive-lobbies (find-inactive-lobbies lobbies now time-inactive)]
    (let [db (update db :lobbies #(apply dissoc % (map :gameid inactive-lobbies)))
          fx (-> [[:lobby/broadcast-list db]]
                 (concat
                   (for [lobby inactive-lobbies]
                     [:lobby/timeout lobby])
                   (for [lobby inactive-lobbies]
                     [:lobby/close-lobby [system-db lobby]]))
                 (vec))]
      {:db db
       :fx fx})))

(executor/reg-event-fx
  :lobby/clean-up
  [executor/unwrap (executor/inject-cofx :inst/now)]
  #'handle-lobby-clean-up)
