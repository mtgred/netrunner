(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require [org.httpkit.client :as http]
            [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [throttler.core :refer [throttle-fn]]
            [clojure.string :as string]
            [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint] :as pprint]
            [cheshire.core :as json]
            [tasks.utils :refer [slugify]]))

(declare faction-map)

(def ^:const base-url "http://www.netrunnerdb.com/api/2.0/public/")
(def ^:const cgdb-image-url "https://www.cardgamedb.com/forums/uploads/an/")
(def ^:const nrdb-image-url "https://netrunnerdb.com/card_image/")

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
   :ffg_id identity
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
   :strength  (fn [[k v]] [:strength (if (nil? v) 0 v)])
   :trash_cost (rename :trash)
   :deck_limit (rename :limited)
   :quantity (rename :packquantity)
   :rotated  identity
   :image_url identity
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
  {:cycle {:path "cycles" :fields cycle-fields :collection "cycles"}
   :mwl   {:path "mwl"    :fields mwl-fields   :collection "mwl"}
   :set   {:path "packs"  :fields set-fields   :collection "sets"}
   :card  {:path "cards"  :fields card-fields  :collection "cards"}
   :config {:collection "config"}})

(defn- translate-fields
  "Modify NRDB json data to our schema"
  [fields data]
  (reduce-kv (fn [m k v]
               (if (contains? fields k)
                 (let [[new-k new-v] ((get fields k) [k v])]
                   (assoc m new-k new-v))
                 m))
             {} data))

(defn- parse-response
  "Parse the http response sent from NRDB"
  [body fields]
  (->> body
    (#(json/parse-string % true))
    :data
    (map (partial translate-fields fields))))

(defn download-nrdb-data
  "Translate data from NRDB"
  [path fields]
  (println "Downloading" path)
  (let [{:keys [status body error] :as resp} @(http/get (str base-url path))]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (= 200 status) (parse-response body fields)
      :else (throw (Exception. (str "Failed to download file, status " status))))))

(defn read-local-data
  "Translate data read from local files"
  [base-path filename fields]
  (let [filepath (str base-path "/" filename ".json")
        _ (println "Reading" filepath)
        content (slurp filepath)
        wrapped (str "{\"data\": " content "}")]
    (parse-response wrapped fields)))

(defn read-card-dir
  [base-path _ fields]
  (let [dirpath (str base-path "/pack")
        _ (println "Reading card directory" dirpath)
        files (mapv str (filter #(.isFile %) (file-seq (clojure.java.io/file dirpath))))
        json-files (filter #(string/ends-with? % ".json") files)
        contents (map slurp json-files)
        parsed (map #(json/parse-string % true) contents)
        combined (flatten parsed)
        wrapped (json/generate-string {:data combined})]
    (parse-response wrapped fields)))

(defn replace-collection
  "Remove existing collection and insert new data"
  [collection data]
  (mc/remove db collection)
  (mc/insert-batch db collection data))

(defn- make-map-by-code
  "Make a map of the items in the list using the :code as the key"
  [l]
  (reduce #(assoc %1 (:code %2) %2) {} l))

(defn add-set-fields
  "Add additional fields to the set documents"
  [cycle-map s]
  (let [c (cycle-map (:cycle_code s))]
    (assoc s
           :rotated (:rotated c)
           :cycle_position (:position c)
           :cycle (:name c))))

(defn- prune-null-fields
  "Remove specified fields if the value is nil"
  [c fields]
  (reduce (fn [acc k]
            (if (nil? (c k))
              (dissoc acc k)
              acc))
          c fields))

(defn- make-image-url
  "Create a URI to the card in CardGameDB"
  [card set]
  (if (:ffg_id set)
    (str cgdb-image-url "med_ADN" (:ffg_id set) "_" (:number card) ".png")
    (str nrdb-image-url (:code card) ".png")))

(defn- get-uri
  "Figure out the card art image uri"
  [card set]
  (if (contains? card :image_url)
    (:image_url card)
    (make-image-url card set)))

(defn- add-card-fields
  "Add additional fields to the card documents"
  [set-map c]
  (let [s (set-map (:set_code c))]
    (-> c
      (prune-null-fields [:influencelimit :strength :factioncost])
      (assoc :setname (:name s)
             :cycle_code (:cycle_code s)
             :rotated (:rotated s)
             :image_url (get-uri c s)
             :normalizedtitle (slugify (:title c))))))

(defn fetch-data
  "Read NRDB json data. Modify function is mapped to all elements in the data collection."
  ([download-fn m] (fetch-data download-fn m identity))
  ([download-fn m modify-function] (fetch-data download-fn m modify-function replace-collection))
  ([download-fn {:keys [path fields collection]} modify-function collection-function]
  (let [data-list (->> (download-fn path fields)
                    (map modify-function))]
    (collection-function collection data-list)
    (make-map-by-code data-list))))

(defn rotate-cards
  "Added rotation fields to cards"
  [acc [_title prev curr]]
  (-> acc
    (assoc-in [prev :replaced_by] curr)
    (assoc-in [curr :replaces] prev)))

(defn- card-image-file
  "Returns the path to a card's image as a File"
  [card]
  (io/file "resources" "public" "img" "cards" (str (:code card) ".png")))

(defn- download-card-image
  "Download a single card image from NRDB"
  [card]
  (println "Downloading: " (:title card) "\t\t(" (:image_url card) ")")
  (http/get (:image_url card) {:as :byte-array :timeout 120000}
            (fn [{:keys [status body error]}]
              (case status
                404 (println "No image for card" (:code card) (:title card))
                200 (with-open [w (io/output-stream (.getPath (card-image-file card)))]
                      (.write w body))
                (println "Error downloading art for card" (:code card) error)))))

(def download-card-image-throttled (throttle-fn download-card-image 5 :second))

(defn download-card-images
  "Download card images (if necessary) from NRDB"
  [card-map]
   (let [img-dir (io/file "resources" "public" "img" "cards")
         cards (vals card-map)]
     (when-not (.isDirectory img-dir)
       (println "Creating card images directory [" (.getPath img-dir) "]")
       (.mkdir img-dir))
     (let [missing-cards (remove #(.exists (card-image-file %)) cards)
           missing (count missing-cards)]
       (when (> missing 0)
         (println "Downloading art for" missing "cards...")
         (let [futures (doall (map download-card-image-throttled missing-cards))]
           (doseq [resp futures]
             ; wait for all the GETs to complete
             (:status @resp)))
         (println "Finished downloading card art")))))

(defn fetch-cards
  "Find the NRDB card json files and import them."
  [download-fn {:keys [collection path] :as card-table} sets download-images]
  (let [cards (fetch-data download-fn
                          card-table
                          (partial add-card-fields sets)
                          (fn [c d] true))
        cards-replaced (->> cards
                            vals
                            (group-by :title)
                            (filter (fn [[k v]] (>= (count v) 2)))
                            vals
                            (map (fn [[c1 c2]] [(:title c1)
                                                (if (:rotated c1) (:code c1) (:code c2))
                                                (if (:rotated c1) (:code c2) (:code c1))]))
                            (reduce rotate-cards cards))]
    (doseq [card (vals cards-replaced)]
      (let [file-name (str "data/cards/" (:normalizedtitle card) ".edn")]
        (io/make-parents file-name)
        ;; Below taken from https://coderwall.com/p/mxbxdq/clojure-fast-pretty-print-writes-to-file
        (with-open [w (clojure.java.io/writer file-name)]
          (binding [*out* w
                    clojure.pprint/*print-right-margin* 350]
            (clojure.pprint/write (into (sorted-map) card))))))
    (mc/remove db collection)
    (mc/insert-batch db collection (vals cards-replaced))
    (when download-images
      (download-card-images cards-replaced))
    cards-replaced))

(defn update-config
  "Store import meta info in the db"
  [{:keys [collection]}]
  (mc/update db collection
             {:cards-version {$exists true}}
             {$inc {:cards-version 1}
              $currentDate {:last-updated true}}
             {:upsert true}))
