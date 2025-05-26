(ns nr.local-storage
  "Centralized localStorage management with proper serialization"
  (:require [clojure.string :as str]))

(defn- serialize-value
  "Serialize a value for localStorage storage.
   Handles primitive types as-is, complex types as JSON.
   Note: Sets are converted to vectors due to JSON limitations."
  [v]
  (cond
    (nil? v) nil
    (or (string? v) (number? v) (boolean? v)) (str v)
    :else (.stringify js/JSON (clj->js v))))

(defn- deserialize-value
  "Deserialize a value from localStorage.
   Uses the default-value to determine expected type."
  [stored-value default-value]
  (cond
    ;; No value stored
    (nil? stored-value) default-value

    ;; Simple types
    (string? default-value) stored-value
    (number? default-value) (js/parseFloat stored-value)
    (boolean? default-value) (= stored-value "true")

    ;; Complex types - parse as JSON
    :else (try
            (let [parsed (js->clj (.parse js/JSON stored-value) :keywordize-keys true)]
              ;; Special handling for sets
              (if (set? default-value)
                (set parsed)
                parsed))
            (catch :default _
              default-value))))

(defn save!
  "Save a value to localStorage with proper serialization"
  [k v]
  (if (nil? v)
    (.removeItem js/localStorage k)
    (.setItem js/localStorage k (serialize-value v))))

(defn load
  "Load a value from localStorage with proper deserialization"
  [k default-value]
  (deserialize-value (.getItem js/localStorage k) default-value))

(defn remove!
  "Remove a key from localStorage"
  [k]
  (.removeItem js/localStorage k))

(defn migrate-keys!
  "Migrate old key names to new ones"
  [migrations]
  (doseq [[old-key new-key] migrations]
    (when-let [value (.getItem js/localStorage old-key)]
      (.setItem js/localStorage new-key value)
      (.removeItem js/localStorage old-key))))
