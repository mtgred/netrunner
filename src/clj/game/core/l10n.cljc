(ns game.core.l10n 
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.string :as str]
   [game.core.card :refer [get-title]]
   [game.core.schemas :as schemas]
   [malli.core :as m]
   [noahtheduke.fluent :as fluent]))

(defonce dictionary (atom {}))

#?(:clj (doseq [lang ["en" "fr"]
                :let [content (io/resource (str (io/file "public" "i18n" "messages" (str lang ".ftl"))))]]
          (swap! dictionary assoc lang (fluent/build lang (slurp content)))))

(def target-language "en")

(defn translate
  "cljc"
  ([id] (translate id nil))
  ([id args]
   (let [bundle (get @dictionary target-language)]
     (try (fluent/format bundle id args)
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) ex
            (if (str/starts-with? (ex-message ex) "Missing message for id")
              (fluent/format (get @dictionary "en") id args)
              (throw ex)))))))

(defn join-with-and
  "cljc"
  [ms]
  (str/join (translate :join-with-and) ms))

(defn join-list
  "cljc"
  [ms]
  (->> ms
       (mapv #(if (:cid %) (get-title %) %))
       (str/join (translate :join-list))))

(defonce server-names (atom {}))

(defn format-server-name
  [server-name]
  (or (get-in @server-names [target-language server-name])
      (let [sn (case server-name
                 ("HQ" :hq) "hq"
                 ("R&D" :rd) "rd"
                 ("Archives" :archives) "archives"
                 #_:else
                 (let [s (str server-name)]
                   (cond
                     (str/starts-with? s "Server") (subs s 7)
                     (str/starts-with? s ":remote") (str (inc (parse-long (subs s 7)))))))
            tr (translate :server-name {:server sn})]
        (-> (swap! server-names assoc-in [target-language server-name] tr)
            (get-in [target-language server-name])))))

(defn format-effect-msgs
  "cljc"
  [{effect-type :effect/type :as m}]
  (let [m (reduce-kv
           (fn [m k v]
             (assoc! m (name k)
                     (cond
                       (= :effect/server k) (format-server-name v)
                       (sequential? v) (join-list v)
                       :else v)))
           (transient {})
           m)]
    (translate effect-type (persistent! m))))

(defn build-ability-msg
  [ms]
  (when-let [ms (seq ms)]
    (->> ms
         (mapv format-effect-msgs)
         (join-with-and))))

(defn format-payment-msg
  "cljc"
  [m]
  (translate (:payment/type m) (update-keys m name)))

(defn build-pay-msg
  "cljc"
  [ms]
  (when-let [ms (seq ms)]
    (->> ms
         (mapv format-payment-msg)
         (join-with-and))))

(defn build-base-msg
  "cljc"
  [m]
  (let [do-ability (build-ability-msg (:msg/effect-msgs m))
        pay-msg (build-pay-msg (:msg/payments m))]
    (cond-> (assoc m :do-ability do-ability)
      pay-msg (assoc :payment pay-msg))))

(defn format-ability-msg
  "cljc"
  [m]
  (translate (:msg/type m) m))

(defn build-msg
  "cljc"
  [m]
  (format-ability-msg (build-base-msg m)))

;; input data -> message map

(def EffectMsg
  (m/schema
   [:map-of
    [:qualified-keyword {:namespace :effect}]
    :any]))

(defn process-effect-msgs
  [effect-msgs]
  (let [fs (cond (sequential? effect-msgs) (vec effect-msgs)
                 (map? effect-msgs) [effect-msgs])]
    (not-empty (mapv #(schemas/assert % EffectMsg) fs))))

(defn payment->msg
  [{:paid/keys [type value targets side] :as payment}]
  (schemas/assert payment schemas/Payment)
  (cond
    (= :credit type)
    (if-let [picks (seq (filter #(contains? % :pick-counters/type) targets))]
       (vec (for [pick picks]
              (case (:pick-counters/type pick)
                :card {:payment/type "payment-hosted-credit"
                       :payment/value (:value pick)
                       :payment/title (:title pick)}
                :bad-publicity {:payment/type "payment-bad-publicity"
                                :payment/value (:value pick)}
                :credit-pool {:payment/type "payment-credit-pool"
                              :payment/value (:value pick)})))
      [{:payment/type "payment-credit"
        :payment/value value}])
    (= :trash-from-hand type)
    [{:payment/type (if (= :corp side)
                      "payment-trash-from-hq"
                      "payment-trash-from-grip")
      :payment/count (count targets)
      :payment/titles (mapv get-title targets)}]
    :else
    [{:payment/type (str "payment-" (name type))
      :payment/value value}]))

(defn process-payments
  [payments]
  (when-let [ps (seq payments)]
    (into [] (mapcat payment->msg) ps)))

(defn ->base-msg
  [{:msg/keys [effect-msgs payments] :as base-msg}]
  (let [effect-msgs (process-effect-msgs effect-msgs)
        payments (process-payments payments)
        args (dissoc base-msg :msg/effect-msgs :msg/payments)]
    (cond-> {:msg/effect-msgs effect-msgs}
      payments (assoc :msg/payments payments)
      (not-empty args) (merge args))))

(defn ->use-card-msg
  ([card effect-msgs] (->use-card-msg card effect-msgs nil nil))
  ([card effect-msgs payments] (->use-card-msg card effect-msgs payments nil))
  ([card effect-msgs payments args]
   (cond-> (->base-msg {:msg/effect-msgs effect-msgs
                        :msg/payments payments
                        :msg/type (if (seq payments) :pay-use-card :use-card)
                        :title (get-title card)})
     (map? args) (merge args))))

(defn msg-map->effect-msg [m]
  (when m
    (if (keyword? m)
      {:effect/type m}
      (schemas/assert m EffectMsg))))

(defmacro simple-msg
  "wraps `game.macros/effect`, calls `->use-card-msg` with each opts as effect-msg map.

  can also be given a keyword, which is wrapped as `{:effect/type opt}`."
  [& opts]
  `(game.macros/effect
    (->use-card-msg ~'card (keep msg-map->effect-msg [~@opts]))))

(defmacro msg-with-cost
  "wraps `game.macros/effect`, calls `->use-card-msg` with each opts as effect-msg map.

  can also be given a keyword, which is wrapped as `{:effect/type opt}`."
  [& opts]
  `(game.macros/effect
    (->use-card-msg ~'card (keep msg-map->effect-msg [~@opts]) (vals (:cost-paid ~'eid)))))
