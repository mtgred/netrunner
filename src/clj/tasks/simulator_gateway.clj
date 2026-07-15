(ns tasks.simulator-gateway
  "Exports the standalone simulator's checked-in System Gateway beginner manifest."
  (:require
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [jinteki.preconstructed :as preconstructed]))

(def default-card-data "data/cards.edn")
(def default-output "simulator/crates/netrunner-sim-gateway/data/system-gateway-beginner.json")

(defn- card-def-id
  [card]
  (Integer/parseInt (:code card)))

(defn- card-ref
  [card]
  (array-map
    :card_def_id (card-def-id card)
    :title (:title card)))

(defn- deck-entry
  [cards-by-title {:keys [qty card]}]
  (let [definition (or (get cards-by-title card)
                       (throw (ex-info "Preconstructed card is missing from card data"
                                       {:title card})))]
    (array-map
      :quantity qty
      :card_def_id (card-def-id definition)
      :title (:title definition))))

(defn- deck
  [cards-by-title {:keys [name identity cards]}]
  (let [identity-card (or (get cards-by-title (:title identity))
                          (throw (ex-info "Preconstructed identity is missing from card data"
                                          {:title (:title identity)})))]
    (when-not (= (:code identity) (card-def-id identity-card))
      (throw (ex-info "Preconstructed identity code differs from card data"
                      {:title (:title identity)
                       :preconstructed-code (:code identity)
                       :card-data-code (:code identity-card)})))
    (array-map
      :name name
      :side (:side identity)
      :identity (card-ref identity-card)
      :cards (mapv #(deck-entry cards-by-title %) cards))))

(defn- card-definition
  [card]
  (array-map
    :card_def_id (card-def-id card)
    :title (:title card)
    :side (:side card)
    :card_type (:type card)))

(defn manifest
  [cards]
  (let [cards-by-title (into {} (map (juxt :title identity) cards))
        corp (deck cards-by-title preconstructed/gateway-beginner-corp)
        runner (deck cards-by-title preconstructed/gateway-beginner-runner)
        selected-ids (into #{}
                           (concat
                             [(get-in corp [:identity :card_def_id])
                              (get-in runner [:identity :card_def_id])]
                             (map :card_def_id (:cards corp))
                             (map :card_def_id (:cards runner))))
        selected-cards (->> cards
                            (filter #(contains? selected-ids (card-def-id %)))
                            (sort-by card-def-id)
                            (mapv card-definition))]
    (array-map
      :schema_version 1
      :agenda_point_target 6
      :corp corp
      :runner runner
      :cards selected-cards)))

(defn manifest-json
  [cards]
  (str (json/generate-string (manifest cards) {:pretty true}) "\n"))

(defn- load-cards
  [path]
  (when-not (.exists (io/file path))
    (throw (ex-info (str "Missing " path "; run `lein fetch --no-db --no-card-images` first")
                    {:path path})))
  (edn/read-string (slurp path)))

(defn- check-manifest!
  [expected output]
  (let [actual (slurp output)]
    (when-not (= expected actual)
      (throw (ex-info (str output " has drifted; run `lein simulator-gateway`")
                      {:output output})))
    (println output "is current")))

(defn command
  [& args]
  (let [check? (= ["--check"] args)
        _ (when-not (or (empty? args) check?)
            (throw (ex-info "Usage: lein simulator-gateway [--check]" {:args args})))
        output default-output
        rendered (manifest-json (load-cards default-card-data))]
    (if check?
      (check-manifest! rendered output)
      (do
        (io/make-parents output)
        (spit output rendered)
        (println "Wrote" output)))))
