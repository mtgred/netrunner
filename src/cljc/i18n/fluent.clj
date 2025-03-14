(ns i18n.fluent
  (:refer-clojure :exclude [format])
  (:require
   [clojure.core :as cc])
  (:import
   (fluent.bundle FluentBundle FluentBundle$Builder FluentResource)
   (fluent.functions.cldr CLDRFunctionFactory)
   (fluent.syntax.AST Pattern)
   (fluent.syntax.parser FTLParser FTLStream ParseException)
   (java.util Locale Map Optional)))

(set! *warn-on-reflection* true)

(defn build
  [locale-str ^String resource]
  (let [locale-str (if (= "la-pig" locale-str) "en" locale-str)
        locale (Locale/forLanguageTag locale-str)
        builder (FluentBundle/builder locale CLDRFunctionFactory/INSTANCE)
        ftl-res (FTLParser/parse (FTLStream/of resource))]
    (when (FluentResource/.hasErrors ftl-res)
      (let [errors (.errors ftl-res)
            err (first errors)]
        (throw (ex-info (str "Error adding resource: " (ex-message err))
                        {:locale locale-str
                         :errors (mapv ex-message errors)}
                        err))))
    (FluentBundle$Builder/.addResource builder ftl-res)
    (try (FluentBundle$Builder/.build builder)
         (catch ParseException ex
           (println (ex-message ex))))))

(defn ^:private k->str
  [k]
  (cond (string? k) k
        (keyword? k) (str (symbol k))
        (symbol? k) (str k)
        :else (throw (ex-info (cc/format "Given wrong type: '%s' expected string or keyword" (type k))
                              {:key k}))))

(defn format
  ([bundle id] (format bundle (k->str id) nil))
  ([^FluentBundle bundle id args]
   (when bundle
     (let [id (k->str id)
           args (if args (update-keys args k->str) (Map/of))]
       (-> (FluentBundle/.getMessagePattern bundle id)
           (Optional/.flatMap #(FluentBundle/.formatPattern bundle ^Pattern % ^Map args))
           (Optional/.orElseThrow
            (fn [] (ex-info (cc/format "Error in id: '%s'" id) {:id id
                                                                :args args}))))))))

(comment
  (let [input "# Simple things are simple.
-poop = poop
hello-world = Hello, world!
hello-user = Hello, {$user-name}!

# Complex things are possible.
shared-photos =
    {$user-name} {$photo-count ->
        [one] added a new photo
       *[other] added {$photo-count} new photos
    } to {$user-gender ->
        [male] his stream
        [female] her stream
       *[other] their stream
    }."
        bundle (build "en" input)]
    (println (format bundle "hello-world"))
    (println (format bundle :hello-user {:uer-name "Noah"}))))
