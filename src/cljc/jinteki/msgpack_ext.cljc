(ns jinteki.msgpack-ext
  (:require
   [cljc.java-time.local-date-time :as ldt]
   [taoensso.msgpack.common :as msgpack]
   #?@(:cljs [[java.time :refer [LocalDateTime]]]))
  #?(:clj (:import (java.time LocalDateTime))))

(msgpack/extend-packable 100 LocalDateTime
  (pack [x]
    #?(:clj (.getBytes (str x) "UTF-8")
       :cljs (.encode (js/TextEncoder.) (str x))))
  (unpack [ba]
    (ldt/parse
     #?(:clj (String. ^bytes ba "UTF-8")
        :cljs (.decode (js/TextDecoder.) ba)))))
