(ns executor.fx
  "shamelessly stolen from/inspired by re-frame: https://github.com/day8/re-frame
  licensed under MIT
  source: https://github.com/day8/re-frame/blob/master/src/re_frame/fx.cljc

  REMEMBER: fx means side-effects"
  (:require
   [executor.db :refer [app-db]]
   [executor.interceptor :refer [->interceptor]]
   [executor.loggers :refer [console]]
   [executor.registrar :as registrar :refer [get-handler register-handler]]
   [malli.core :as m]
   [malli.error :as me]))

;; -- Registration ------------------------------------------------------------

(def kind :fx)
(assert (registrar/kinds kind))

(defn reg-fx
  [id handler]
  (register-handler kind id handler))

;; -- Interceptor -------------------------------------------------------------

(defn do-fx-after
  [context]
  (let [effects            (:effects context)
        effects-without-db (dissoc effects :db)]
    ;; :db effect is guaranteed to be handled before all other effects.
    (when-let [new-db (:db effects)]
      ((get-handler kind :db false) new-db))
    (doseq [[effect-key effect-value] effects-without-db]
      (if-let [effect-fn (get-handler kind effect-key false)]
        (effect-fn effect-value)
        (console :warn "executor: no handler registered for effect:" effect-key ". Ignoring.")))))

(def do-fx
  "An interceptor whose `:after` actions the contents of `:effects`. As a result,
  this interceptor is Domino 3.

  This interceptor is silently added (by reg-event-db etc) to the front of
  interceptor chains for all events.

  For each key in `:effects` (a map), it calls the registered `effects handler`
  (see `reg-fx` for registration of effect handlers).

  So, if `:effects` was:
      {:dispatch  [:hello 42]
       :db        {...}
       :undo      \"set flag\"}

  it will call the registered effect handlers for each of the map's keys:
  `:dispatch`, `:undo` and `:db`. When calling each handler, provides the map
  value for that key - so in the example above the effect handler for :dispatch
  will be given one arg `[:hello 42]`.

  You cannot rely on the ordering in which effects are executed, other than that
  `:db` is guaranteed to be executed first."
  (->interceptor
    :id :do-fx
    :after #'do-fx-after))

;; -- Builtin Effect Handlers  ------------------------------------------------

(do
  (def FxSchema
    (m/schema
      [:* [:maybe [:cat qualified-keyword? [:? any?]]]]))
  (def validate-fx (m/validator FxSchema))
  (def explain-fx (m/explainer FxSchema)))

(comment
  (validate-fx [nil [:a/a] [:a/a 1]])
  (explain-fx [nil [:a/a] [:a/a 1]])
  )

;; :fx
;;
;; Handle one or more effects. Expects a collection of vectors (tuples) of the
;; form [effect-key effect-value]. `nil` entries in the collection are ignored
;; so effects can be added conditionally.
;;
;; usage:
;;
;; {:fx [[:dispatch [:event-id "param"]]
;;       nil
;;       [:http-xhrio {:method :post
;;                     ...}]]}
;;
(reg-fx
  :fx
  (fn handle-fx [seq-of-effects]
    (if-not (validate-fx seq-of-effects)
      (console :warn "executor: \":fx\"" seq-of-effects "contains error" (me/humanize (explain-fx seq-of-effects)) ". Ignoring.")
      (doseq [[effect-key effect-value] (remove nil? seq-of-effects)]
        (if (= :db effect-key)
          (console :warn "executor: \":fx\" effect should not contain a :db effect. Ignoring.")
          (if-let [effect-fn (get-handler kind effect-key false)]
            (effect-fn effect-value)
            (console :warn "executor: in \":fx\" effect found" effect-key "which has no associated handler. Ignoring.")))))))

;; :dispatch
;;
;; `dispatch` one event. Expects a single vector.
;;
;; usage:
;;   {:dispatch [:event-id "param"] }
;;
;; TODO: figure out how to do this part
(reg-fx
  :dispatch
  (fn [value]
    (if-not (vector? value)
      (console :error "executor: ignoring bad :dispatch value. Expected a vector, but got:" value)
      value ;; TODO
      ; (router/dispatch value)
      )))

;; :db
;;
;; reset! app-db with a new value. `value` is expected to be a map.
;;
;; usage:
;;   {:db  {:key1 value1 key2 value2}}
;;
(reg-fx
  :db
  (fn update-app-db! [value]
    (when-not (identical? @app-db value)
      (reset! app-db value))))
