(ns jinteki.msgpack-ext
  (:require
   [cljc.java-time.local-date-time :as ldt]
   [taoensso.msgpack.common :as msgpack]
   #?@(:cljs [[cljc.java-time.instant :as inst]
              [cljs.reader :as reader]
              [java.time :refer [LocalDateTime]]
              ;; load taoensso.msgpack.impl to override -1
              [taoensso.msgpack]]))
  #?(:clj (:import (java.time LocalDateTime))))

#?(:clj (set! *warn-on-reflection* true))

(msgpack/extend-packable 100 LocalDateTime
  (pack [x]
    #?(:clj (.getBytes (str x) "UTF-8")
       :cljs (.encode (js/TextEncoder.) (str x))))
  (unpack [ba]
    (ldt/parse
     #?(:clj (String. ^bytes ba "UTF-8")
        :cljs (.decode (js/TextDecoder.) ba)))))

;; read serialized #inst as cljc.java-time.instant in cljs prod (msgpack) and dev (edn)
;; note: we also use time-literals.read-write/print-time-literals-cljs! to serialize
;; #inst as #time/instant, but due to how clojurescripts' clj files also define
;; a printer for java.time.Instant, it can end up still as #inst instead in edn.

#?(:cljs
   ;; msgpack has timestamp predefined as -1
   ;; https://github.com/msgpack/msgpack/blob/master/spec.md#extension-types
   ;; sente's msgpack impl uses -1 for both java.util.Date and java.time.Instant
   ;; https://github.com/taoensso/sente/blob/1442b8b9becf9a94a159f0f15f30ad3bda61ea80/src/taoensso/msgpack/impl.clj#L455-L464
   ;; bind the original fn that sente's msgpack uses to unpack its timestamps, then
   ;; override just the unpacking so it unpacks to cljc.java-time.instant
   (let [unpack-date (get-method msgpack/unpack-ext -1)]
     (msgpack/extend-packable -1 nil
       nil
       (unpack [ba]
         (inst/of-epoch-milli (.getTime (unpack-date -1 ba)))))))

#?(:cljs
   ;; read edn #inst tags as cljc.java-time.instant instead of js/Date
   (reader/register-tag-parser! 'inst #(inst/of-epoch-milli (.getTime (reader/parse-timestamp %)))))
