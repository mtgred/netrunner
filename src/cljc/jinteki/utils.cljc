(ns jinteki.utils)

(defn str->int [s]
  #?(:clj  (java.lang.Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn side-from-str [side-str]
  (keyword (.toLowerCase side-str)))