(ns executor.events
  "shamelessly stolen from/inspired by re-frame: https://github.com/day8/re-frame
  licensed under MIT
  source: https://github.com/day8/re-frame/blob/master/src/re_frame/events.cljc"
  (:require
   [executor.interceptor :as interceptor]
   [executor.loggers :refer [console]]
   [executor.registrar :as registrar :refer [get-handler register-handler]]
   [malli.core :as m]
   [malli.error :as me]))

(def kind :event)
(assert (registrar/kinds kind))

(defn- make-chain [interceptors]
  (->> interceptors flatten (remove nil?)))

(defn- flatten-and-remove-nils
  "`interceptors` might have nested collections, and contain nil elements.
  return a flat collection, with all nils removed.
  This function is 9/10 about giving good error messages."
  [id interceptors]
  (when-not (coll? interceptors)
    (console :error
             "executor: when registering" id
             ", expected a collection of interceptors, got:" interceptors))
  (let [chain (make-chain interceptors)]
    (when (empty? chain)
      (console :error
               "executor: when registering" id
               ", given an empty interceptor chain"))
    (when-let [not-i (first (remove interceptor/validate-interceptor chain))]
      (if (fn? not-i)
        (console :error
                 "executor: when registering" id
                 ", got a function instead of an interceptor."
                 "Did you provide old style middleware by mistake? Got:" not-i)
        (console :error
                 "executor: when registering" id
                 ", expected interceptors, but got:" not-i)))
    chain))

(defn register
  "Associate the given event `id` with the given collection of `interceptors`.

   `interceptors` may contain nested collections and there may be nils
   at any level,so process this structure into a simple, nil-less vector
   before registration.

   Typically, an `event handler` will be at the end of the chain (wrapped
   in an interceptor)."
  [id interceptors]
  (register-handler kind id (flatten-and-remove-nils id interceptors)))

;; -- handle event --------------------------------------------------------------------------------

(do
  (def EventSchema
    (m/schema
      [:catn
       [:kw keyword?]
       [:args [:* any?]]]))
  (def validate-event (m/validator EventSchema))
  (def explain-event (m/explainer EventSchema)))

(defn handle
  "Given an event vector `event-v`, look up the associated interceptor chain, and execute it."
  [event-v]
  (when-not (validate-event event-v)
    (console :error
             "executor: Event vector is wrong:"
             (me/humanize (explain-event event-v))))
  (let [event-id (nth event-v 0)]
    (when-let [interceptors (get-handler kind event-id true)]
      (interceptor/execute event-v interceptors))))
