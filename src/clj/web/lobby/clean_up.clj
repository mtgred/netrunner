(ns web.lobby.clean-up
  (:require
   [cljc.java-time.instant :as inst]
   [executor.core :as executor]))

(defn handle-lobby-clean-up
  [{{lobbies :lobbies :as db} :db now :inst/now}
   {system-db :system/db time-inactive :time-inactive}]
  (let [inactive-lobbies
        (filter
          (fn [lobby]
            (inst/is-after now (inst/plus-seconds (:last-update lobby) time-inactive)))
          (vals lobbies))]
    (when (seq inactive-lobbies)
      (let [db (update db :lobbies #(apply dissoc % (keep :gameid inactive-lobbies)))
            fx (concat [[:lobby/broadcast-list db]]
                       (for [lobby inactive-lobbies]
                         [:lobby/timeout lobby])
                       (for [lobby inactive-lobbies]
                         [:lobby/close-lobby [system-db lobby]]))]
        {:db db
         :fx fx}))))

(executor/reg-event-fx
  :lobby/clean-up
  [executor/unwrap (executor/inject-cofx :inst/now)]
  #'handle-lobby-clean-up)
