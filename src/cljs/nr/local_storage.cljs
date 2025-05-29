(ns nr.local-storage
  "Centralized localStorage management with proper serialization"
  (:require [clojure.string :as str]
            [jinteki.settings :as settings]))

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

(defn update-local-storage-settings!
  "Update localStorage settings based on sync preferences.
   - Removes all sync settings (they belong in database only)
   - Optionally saves local-only settings from provided settings-map
   - Handles localStorage unavailability gracefully"
  ([]
   (update-local-storage-settings! nil))
  ([settings-map]
   (try
     (doseq [{:keys [key sync?]} settings/all-settings]
       (let [storage-key (name key)]
         (if sync?
           ;; Remove database-sourced settings from localStorage
           (.removeItem js/localStorage storage-key)
           ;; Save local-only settings to localStorage (if provided)
           (when-let [value (get settings-map key)]
             (save! storage-key value)))))
     (catch :default e
       (js/console.warn "localStorage not available for settings update:" e)))))

(defn remove-sync-settings!
  "Remove all sync settings from localStorage.
   These should only persist in the database, not localStorage after logout."
  []
  (update-local-storage-settings!))
