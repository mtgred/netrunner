(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require [org.httpkit.client :as http]
            [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [clojure.string :as string]
            [clojure.data :as data]
            [clojure.pprint :refer [pprint] :as pprint]
            [cheshire.core :as json]))

(declare faction-map)

(def ^:const baseurl "http://netrunnerdb.com/api/2.0/public/")
(def ^:const base-path "data/netrunner-cards-json/")

(defmacro rename
  "Rename a card field"
  [new-name]
  `(fn [[k# v#]] [~new-name v#]))

(def cycle-fields
  {
   :code identity
   :name identity
   :position identity
   :size identity
   :rotated identity
   })

(def set-fields
  {
   :name identity
   :date_release (fn [[k v]] [:available (if (nil? v) "4096-01-01" v)])
   :cycle_code identity
   :size (fn [[k v]] [:bigbox (> (or v -1) 20)])
   :code identity
   :position identity
   })

(def mwl-fields
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
  :keywords (rename :subtype)
  :text  identity
  :cost  (fn [[k v]] [:cost (if (nil? v) 0 v)])
  :advancement_cost (rename :advancementcost)
  :agenda_points (rename :agendapoints)
  :base_link (rename :baselink)
  :influence_limit (rename :influencelimit)
  :minimum_deck_size (rename :minimumdecksize)
  :faction_code (fn [[k v]] [:faction (faction-map v)])
  :faction_cost (rename :factioncost)
  :position (rename :number)
  :pack_code (rename :set_code)
  :cycle_code  identity
  :side_code  (fn [[k v]] [:side (string/capitalize v)])
  :uniqueness  identity
  :memory_cost (rename :memoryunits)
  :strength  identity
  :trash_cost (rename :trash)
  :deck_limit (rename :limited)
  :quantity (rename :packquantity)
  :rotated  identity
   })

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

(def tables
  {:cycle {:path "cycles.json" :fields cycle-fields :collection "clj_cycles"}
   :mwl   {:path "mwl.json"    :fields mwl-fields   :collection "clj_mwl"}
   :set   {:path "packs.json"  :fields set-fields   :collection "clj_sets"}
   :card  {:path "pack"        :fields card-fields  :collection "clj_cards"}})

(defn- translate-fields
  "Modify NRDB json data to our schema"
  [fields data]
  (reduce-kv (fn [m k v]
               (if (contains? fields k)
                 (let [[new-k new-v] ((get fields k) [k v])]
                   (assoc m new-k new-v))
                 m))
             {} data))

(defn- read-nrdb-data
  "Translate data from NRDB"
  [path fields]
  (->> path
    (str base-path)
    slurp
    (#(json/parse-string % true))
    (map (partial translate-fields fields))))

(defn- replace-collection
  "Remove existing collection and insert new data"
  [collection data]
  (mc/remove db collection)
  (mc/insert-batch db collection data))

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

;; from http://www.matt-reid.co.uk/blog_post.php?id=69
(defn deaccent
  "Remove diacritical marks from a string"
  [s]
  (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)]
    (string/replace normalized #"\p{InCombiningDiacriticalMarks}+" "")))

(defn- prune-null-fields
  "Remove specified fields if the value is nil"
  [c fields]
  (reduce (fn [acc k]
            (if (nil? (c k))
              (dissoc acc k)
              acc))
          c fields))

(defn- add-card-fields
  "Add additional fields to the card documents"
  [set-map c]
  (let [s (set-map (:set_code c))]
    (-> c
      (prune-null-fields [:influencelimit :strength])
      (assoc :setname (:name s)
             :cycle_code (:cycle_code s)
             :rotated (:rotated s)
             :normalizedtitle (string/lower-case (deaccent (:title c)))))))

(defn fetch-data
  "Read NRDB json data. Modify function is mapped to all elements in the data collection."
  ([m] (fetch-data m identity))
  ([m modify-function] (fetch-data m modify-function replace-collection))
  ([{:keys [path fields collection]} modify-function collection-function]
  (let [data-list (->> (read-nrdb-data path fields)
                    (map modify-function))]
    (collection-function collection data-list)
    (make-map-by-code data-list))))

(defn rotate-cards
  "Added rotation fields to cards"
  [acc [_title prev curr]]
  (-> acc
    (assoc-in [prev :replaced_by] curr)
    (assoc-in [curr :replaces] prev)))

(defn fetch-cards
  "Find the NRDB card json files and import them."
  [{:keys [collection path] :as card-table} sets]
  (let [cards (->> "pack"
                (str base-path)
                clojure.java.io/file
                .list
                (map (partial str "pack/"))
                (map #(fetch-data (assoc card-table :path %)
                                  (partial add-card-fields sets)
                                  (fn [c d] true)))
                (apply merge))
        cards-replaced (->> cards
                         vals
                         (group-by :title)
                         (filter (fn [[k v]] (>= (count v) 2)))
                         vals
                         (map (fn [[c1 c2]] [(:title c1)
                                             (if (:rotated c1) (:code c1) (:code c2))
                                             (if (:rotated c1) (:code c2) (:code c1))]))
                         (reduce rotate-cards cards)
                           )]
    (mc/remove db collection)
    (mc/insert-batch db collection (vals cards-replaced))
    cards-replaced))

(defn fetch
  "Import data from NetrunnerDB"
  []
  (webdb/connect)
  (try
    (let [cycles (fetch-data (:cycle tables))
          mwls (fetch-data (:mwl tables))
          sets (fetch-data (:set tables) (partial add-set-fields cycles))
          cards (fetch-cards (:card tables) sets)]
      (println (count cycles) "cycles imported")
      (println (count sets) "sets imported")
      (println (count mwls) "MWL versions imported")
      (println (count cards) "cards imported"))
    (catch Exception e (println "Import data failed:" (.getMessage e)))
    (finally (webdb/disconnect))))

(defn compare-collections
  [c1 c2 k]
  (if (not= (mc/count db c1) (mc/count db c2))
    (do
      (println "Different number of elements in collections")
      false)
    (let [c1-data (sort-by k (map #(into (sorted-map) %) (map #(dissoc % :_id) (mc/find-maps db c1))))
          c2-data (sort-by k (map #(into (sorted-map) %) (map #(dissoc % :_id) (mc/find-maps db c2))))
          zipped (filter (fn [[x y]] (not= x y)) (map vector c1-data c2-data))]
      (if (not-empty zipped)
        (do
          (println "First mismatch:")
          (pprint (first zipped))
          false)
        true))))

(defn test-import
  []
  (println "MWL")
  (if (compare-collections "mwl" "clj_mwl" :name)
    (println "\tOK")
    (println "\tFailed"))
  (println "Cycles")
  (if (compare-collections "cycles" "clj_cycles" :name)
    (println "\tOK")
    (println "\tFailed"))
  (println "Sets")
  (if (compare-collections "sets" "clj_sets" :name)
    (println "\tOK")
    (println "\tFailed"))
  (println "Cards")
  (if (compare-collections "cards" "clj_cards" :code)
    (println "\tOK")
    (println "\tFailed"))
  )
