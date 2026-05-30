(ns game.core.schemas
  "this must not pull in any game.core namespaces, it's gotta be standalone"
  (:refer-clojure :exclude [assert])
  (:require
   [clojure.string :as str]
   [malli.core :as m]
   [malli.error :as me]))

(defmacro assert
  "malli schema assert that throws an ex-info. schema comes last to allow for threading"
  [value schema]
  `(let [schema# ~schema
         value# ~value]
     (if (m/validate schema# value#)
       value#
       (let [msg# (->> (me/humanize (m/explain schema# value#))
                       (str/join \newline))]
         (throw (ex-info msg# {:schema '~(symbol (resolve schema))
                               :value value#}))))))

(def Payment
  (m/schema
   [:map
    [:paid/type :keyword]
    [:paid/msg {:optional true} :string]
    [:paid/value :some]
    [:paid/targets {:optional true} [:maybe [:sequential :some]]]]))
