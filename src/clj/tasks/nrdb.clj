(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require [org.httpkit.client :as http]
            [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [clojure.string :as string]
            [cheshire.core :as json]))

(defn rename
  "Rename field"
  [new-name [k v]]
  [new-name v])

;; from http://www.matt-reid.co.uk/blog_post.php?id=69
(defn deaccent
  "Remove diacritical marks from a string"
  [s]
  (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)]
    (string/replace normalized #"\p{InCombiningDiacriticalMarks}+" "")))

(def ^:const faction-map
  {
  "haas-bioroid"  "Haas-Bioroid"
  "jinteki"  "Jinteki"
  "nbn"  "NBN"
  "weyland-consortium"  "Weyland Consortium"
  "anarch"  "Anarch"
  "criminal"  "Criminal"
  "shaper"  "Shaper"
  "adam"  "Adam"
  "sunny-lebeau"  "Sunny Lebeau"
  "apex"  "Apex"
  "neutral-runner"  "Neutral"
  "neutral-corp"  "Neutral"
   })

(def ^:const baseurl "http://netrunnerdb.com/api/2.0/public/")

(def ^:const cycle-fields
  {
   :code identity
   :name identity
   :position identity
   :side identity
   :rotated identity
   })

(def ^:const set-fields
  {
   :name identity
   :date_release (fn [[k v]] [:available (if (nil? v) "4096-01-01" v)])
   :cycle_code identity
   :size (fn [[k v]] [:bigbox (< (or v 999) 20)])
   :code identity
   :position identity
   })

(def ^:const mwl-fields
  {
  :name identity
  :code identity
  :date_start identity
  :cards identity
   })

(def card-fields
  {
  :code identity
  :title identity
  :type_code  (fn [[k v]] [:type (if (= v "ice") "ICE" (string/capitalize v))])
  :keywords (partial rename :subtype)
  :text  identity
  :cost  (fn [[k v]] [:cost (if (nil? v) 0 v)])
  ;; :advancement_cost  rename("advancementcost")
  ;; :agenda_points  rename("agendapoints")
  ;; :base_link  rename("baselink")
  ;; :influence_limit  rename("influencelimit")
  ;; :minimum_deck_size  rename("minimumdecksize")
  :faction_code (fn [[k v]] [:faction (faction-map v)])
  ;; :faction_cost  rename("factioncost"), # influenc
  ;; :position  rename("number")
  :pack_code identity
  :cycle_code  identity
  :side_code  (fn [[k v]] [:side (string/capitalize v)])
  :uniqueness  identity
  ;; :memory_cost  rename("memoryunits")
  :strength  identity
  ;; :trash_cost  rename("trash")
  ;; :deck_limit  rename("limited")
  ;; :quantity  rename("packquantity")
  :rotated  identity
   })

(defn- translate-fields
  "Modify NRDB json data to our schema"
  [fields data]
  (reduce-kv (fn [m k v]
               (if (contains? fields k)
                 (let [[new-k new-v] ((get fields k) [k v])]
                   (assoc m new-k new-v))
                 m))
             {} data))

(defn- get-data
  "Fetch json data from NRDB"
  [path]
  (let [{:keys [status headers body error] :as resp} @(http/get (str baseurl path))]
    (if (or error (not= status 200))
      (do
        (println "Failed to fetch data from NRDB" path ", exception: " error " Status:" status)
        nil)
      (:data (json/parse-string body true)))))

(defn- import-data
  "Download and translate data from NRDB"
  [path fields]
  (->> path
    get-data
    (map (partial translate-fields fields))))

(defn- replace-collection
  "Remove existing collection and insert new data"
  [collection data]
  (when-not (nil? data)
    (mc/remove db collection)
    (mc/insert-batch db collection data)))

(defn- make-map-by-code
  "Make a map of the items in the list using the :code as the key"
  [l]
  (reduce #(assoc %1 (:code %2) %2) {} l))

(defn- add-set-fields
  "Add additional fields to the set documents"
  [cycle-map s]
  (let [c (cycle-map (:cycle_code s))]
    (assoc s
           :rotated (:rotated c)
           :cycle_position (:position c)
           :cycle (:name c))))

(defn- add-card-fields
  "Add additional fields to the card documents"
  [set-map c]
  (let [s (set-map (:pack_code c))]
    (assoc c :setname (:name s)
           :cycle_code (:cycle_code s)
           :rotated (:rotated s)
           :normalized_title (string/lower-case (deaccent (:title c)))
           :set_code (:pack_code c))))

(defn fetch-data
  "Import json data from NRDB. Modify function is mapped to all elements in the data collection."
  ([path fields collection] (fetch-data path fields collection identity))
  ([path fields collection modify-function]
  (let [data-list (->> (import-data path fields)
                    (map modify-function))]
    (replace-collection collection data-list)
    (make-map-by-code data-list))))

(defn fetch
  "Import data from NetrunnerDB"
  []
  (webdb/connect)
  (let [cycle-future (future (fetch-data "cycles" cycle-fields "clj_cycles"))
        mwl-future (future (fetch-data "mwl" mwl-fields "clj_mwl"))
        set-future (future (fetch-data "packs" set-fields "clj_sets" (partial add-set-fields @cycle-future)))
        card-future (future (fetch-data "cards" card-fields "clj_cards" (partial add-card-fields @set-future)))]
    (println (count @cycle-future) "cycles imported")
    (println (count @set-future) "sets imported")
    (println (count @mwl-future) "MWL versions imported")
    (println (count @card-future) "cards imported")
    ))
