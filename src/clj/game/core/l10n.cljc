(ns game.core.l10n
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.string :as str]
   [game.core.card :refer [get-title]]
   [game.core.schemas :as schemas :refer [EffectMsg]]
   [game.core.to-string :refer [card-str-edn]]
   [noahtheduke.fluent :as fluent]))

;; formatting/display

(defonce dictionary (atom {}))

#?(:clj (doseq [lang ["en" "fr"]
                :let [content (io/resource (str (io/file "public" "i18n" "messages" (str lang ".ftl"))))]]
          (swap! dictionary assoc lang (fluent/build lang (slurp content)))))

(def target-language "en")

(defn translate
  "cljc"
  ([id] (translate id nil))
  ([id args]
   (let [d @dictionary
         bundle (d target-language)]
     (try (fluent/format bundle id args)
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) ex
            (if (str/starts-with? (ex-message ex) "Missing message for id")
              (fluent/format (d "en") id args)
              (throw ex)))))))

(defn join-with-and
  "cljc"
  [ms]
  (str/join (translate :join-with-and) ms))

(defn get-card-title
  [m]
  (if (:cid m) (get-title m) m))

(defn join-list
  "cljc"
  [ms]
  (->> ms
       (mapv get-card-title)
       (str/join (translate :join-list))))

(defonce server-names (atom {}))
(reset! server-names {})

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
                     (str/starts-with? s ":remote") (str (parse-long (subs s 7))))))
            tr (translate :server-name {:server sn})]
        (-> (swap! server-names assoc-in [target-language server-name] tr)
            (get-in [target-language server-name])))))

(defn format-card-str
  [state card]
  (let [m (card-str-edn state card)]
    (translate (:card/str m) m)))

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


(defn format-msg-args
  [state m]
  (let [new-m (reduce-kv
               (fn [m k v]
                 (assoc! m (name k)
                         (cond
                           (#{:msg/payments
                              :effect/card-str :effect/card-strs
                              :msg/card-str :msg/card-strs} k) nil
                           (= :effect/server k) (format-server-name v)
                           (= :effect/title k) (get-card-title v)
                           (sequential? v) (join-list v)
                           :else v)))
               (transient {}) m)
        card-str (or (:effect/card-str m)
                     (:msg/card-str m))
        card-strs (or (:effect/card-strs m)
                      (:msg/card-strs m))]
    (cond-> new-m
      (:msg/payments m) (assoc! "payment" (build-pay-msg (:msg/payments m)))
      card-str (assoc! "card-str" (format-card-str state card-str))
      card-strs (assoc! "card-strs" (->> card-strs
                                         (mapv #(format-card-str state %))
                                         (str/join (translate :join-list))))
      true (persistent!))))

(defn format-effect-msgs
  "cljc"
  [state {effect-type :effect/type :as m}]
  (translate effect-type (format-msg-args state m)))

(defn build-ability-msg
  [state ms]
  (when-let [ms (seq ms)]
    (->> ms
         (mapv #(format-effect-msgs state %))
         (join-with-and))))

(defn build-msg
  "cljc"
  [state {msg-type :msg/type :as m}]
  (let [m (if (:msg/effect-msgs m)
            (assoc m :do-ability (build-ability-msg state (:msg/effect-msgs m)))
            m)]
    (translate msg-type (format-msg-args state m))))

;; input data -> message map

(defn process-effect-msgs
  [effect-msgs]
  (let [fs (cond (sequential? effect-msgs) (vec effect-msgs)
                 (map? effect-msgs) [effect-msgs])]
    (not-empty (mapv #(schemas/assert % EffectMsg) fs))))

(defn payment->msg
  [{:paid/keys [type value targets side] :as payment}]
  (schemas/assert payment schemas/Payment)
  (case type
    :credit
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
    :x-credits
    [{:payment/type "payment-x-credit"
      :payment/value (:x-value payment)}]
    :trash-from-hand
    [{:payment/type (if (= :corp side)
                      "payment-trash-from-hq"
                      "payment-trash-from-grip")
      :payment/count (count targets)
      :payment/titles (mapv get-title targets)}]
    #_ :else
    [{:payment/type (str "payment-" (name type))
      :payment/value value
      :payment/title (get-title (first targets))
      :payment/titles (when-let [t (not-empty targets)] (mapv get-title t))}]))

(defn process-payments
  [payments]
  (when-let [ps (seq payments)]
    (into [] (mapcat payment->msg) ps)))

(defn ->effect-msg
  [{:msg/keys [effect-msgs payments] :as base-msg}]
  (let [effect-msgs (process-effect-msgs effect-msgs)
        payments (process-payments payments)
        args (dissoc base-msg :msg/effect-msgs :msg/payments)]
    (cond-> {:msg/effect-msgs effect-msgs}
      payments (assoc :msg/payments payments)
      (not-empty args) (merge args))))

(defn msg-map->effect-msg [m]
  (when m
    (if (keyword? m)
      {:effect/type m}
      (schemas/assert m EffectMsg))))

(defn ->use-card-msg
  ([card effect-msgs] (->use-card-msg card effect-msgs nil nil))
  ([card effect-msgs payments] (->use-card-msg card effect-msgs payments nil))
  ([card effect-msgs payments args]
   (cond-> (->effect-msg {:msg/type (if (seq payments) :pay-use-card :use-card)
                          :msg/effect-msgs (vec (keep msg-map->effect-msg effect-msgs))
                          :msg/payments payments
                          :title (get-title card)})
     (map? args) (merge args))))

(defmacro simple-msg
  "wraps `game.macros/effect`, calls `->use-card-msg` with each opts as effect-msg map.

  can also be given a keyword, which is wrapped as `{:effect/type opt}`."
  [& opts]
  `(game.macros/effect
    (->use-card-msg ~'card [~@opts])))

(defmacro msg-with-cost
  "wraps `game.macros/effect`, calls `->use-card-msg` with each opts as effect-msg map.

  can also be given a keyword, which is wrapped as `{:effect/type opt}`."
  [& opts]
  `(game.macros/effect
    (->use-card-msg ~'card [~@opts] (vals (:cost-paid ~'eid)))))
