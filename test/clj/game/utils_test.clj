(ns game.utils-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]))

(defmacro error-wrapper [form]
  `(try ~form
        (catch clojure.lang.ExceptionInfo ex#
          (if (:form (ex-data ex#))
            (let [msg# (ex-message ex#)
                  form# (:form (ex-data ex#))
                  pred# (:pred (ex-data ex#))
                  values# (:values (ex-data ex#))
                  result# (:result (ex-data ex#))]
              (do-report {:type :fail
                          :message msg#
                          :expected form#
                          :actual (list '~'not (cons pred# values#))})
              result#)
            (throw ex#)))))

(defn is'-predicate
  [msg form]
  (let [[pred & args] form
        uses-state? (= 'state (first args))]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (do-report {:type :pass
                     :message ~msg
                     :expected '~form
                     :actual result#})
         (throw (ex-info ~msg {:form '~form
                               :pred '~pred
                               :values (if ~uses-state?
                                         '~(cons 'state (next args))
                                         values#)
                               :result result#})))
       result#)))

(defn is'-any
  [msg form]
  `(let [result# ~form]
     (if result#
       (do-report {:type :pass
                   :message ~msg
                   :expected '~form
                   :actual result#})
       (throw (ex-info ~msg {:form '~form
                             :pred '~'identity
                             :values (list result#)
                             :result result#})))
     result#))

(defmacro is'
  ([form] `(is' ~form nil))
  ([form msg]
   (if (and (sequential? form)
            (function? (first form)))
     (is'-predicate msg form)
     (is'-any msg form))))
