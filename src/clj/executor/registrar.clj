(ns executor.registrar
  "shamelessly stolen from/inspired by re-frame: https://github.com/day8/re-frame
  licensed under MIT
  source: https://github.com/day8/re-frame/blob/master/src/re_frame/registrar.cljc" 
  (:require
   [executor.loggers :refer [console]]))

;; kinds of handlers
(def kinds #{:event :fx :cofx :sub})

(def kind->id->handler (atom {}))

(defn get-handler
  "Get all of the handlers for a kind or a single handler for a kind and id.
  If passed a truthy third param, throws if the handler doesn't exist."
  ([kind] (get @kind->id->handler kind))
  ([kind id] (-> @kind->id->handler
                 (get kind)
                 (get id)))
  ([kind id required?]
   (let [handler (get-handler kind id)]
     (when (and required? (nil? handler))
       (console :error "executor: No" kind "handler registered for" id))
     handler)))

(defn register-handler [kind id handler-fn]
  (assert (kinds kind) "wrong kind")
  (when (get-in @kind->id->handler [kind id])
    (console :warn "re-frame: overwriting" (str kind) "handler for:" id))
  (swap! kind->id->handler assoc-in [kind id] handler-fn)
  handler-fn)
