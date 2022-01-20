(ns executor.cofx
  "shamelessly stolen from/inspired by re-frame: https://github.com/day8/re-frame
  licensed under MIT
  source: https://github.com/day8/re-frame/blob/master/src/re_frame/cofx.cljc"
  (:require
   [executor.db :refer [app-db]]
   [executor.interceptor :refer [->interceptor]]
   [executor.loggers :refer [console]]
   [executor.registrar :as registrar :refer [get-handler register-handler]]))

;; -- Registration ------------------------------------------------------------

(def kind :cofx)
(assert (registrar/kinds kind))

(defn reg-cofx
  [id handler]
  (register-handler kind id handler))

;; -- Interceptor -------------------------------------------------------------

(defn inject-cofx
  "Update :coeffects with associated id handler function (reg-cofx)"
  ([id]
   (->interceptor
     :id     :coeffects
     :before (fn coeffects-before
               [context]
               (if-let [handler (get-handler kind id)]
                 (update context :coeffects handler)
                 (console :error "No cofx handler registered for" id)))))
  ([id value]
   (->interceptor
     :id     :coeffects
     :before (fn coeffects-before
               [context]
               (if-let [handler (get-handler kind id)]
                 (update context :coeffects handler value)
                 (console :error "No cofx handler registered for" id))))))

;; -- Builtin CoEffects Handlers  ---------------------------------------------

;; :db
;;
;; Adds to coeffects the value in `app-db`, under the key `:db`
(reg-cofx
  :db
  (fn db-coeffects-handler
    [coeffects]
    (assoc coeffects :db @app-db)))

;; Because this interceptor is used so much, we reify it
(def inject-db (inject-cofx :db))
