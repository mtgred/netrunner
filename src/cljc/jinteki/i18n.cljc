(ns jinteki.i18n
  (:refer-clojure :exclude [format])
  (:require
   #?(:clj [clojure.java.io :as io])
   #?(:cljs [reagent.core :as r])
   #?(:cljs [nr.appstate :refer [app-state]])
   #?(:cljs [cljs.core.async :refer [take!] :refer-macros [go]])
   #?(:cljs [nr.ajax :refer [GET]])
   [clojure.string :as str]
   [game.core.card :refer [get-title]]
   [game.core.schemas :as schemas :refer [EffectMsg]]
   [game.core.to-string :refer [card-str-edn]]
   [noahtheduke.fluent :as fluent]))

#?(:clj (set! *warn-on-reflection* true))

(def language-cursor
  #?(:clj (delay "en")
     :cljs (r/cursor app-state [:options :language])))

(defonce fluent-dictionary
  #?(:clj (atom nil)
     :cljs (r/atom {})))

(defn insert-lang! [lang content]
  (swap! fluent-dictionary assoc lang {#?@(:clj [:content content])
                                       :ftl (let [lang (if (= "la-pig" lang) "en" lang)]
                                              (fluent/build lang content))}))

#?(:clj
   (defn load-dictionary!
     [dir]
     (when-let [dir (io/file (io/resource dir))]
       ;; List of supported language files (based on what we support in the frontend)
       (let [langs (->> ["ca" "en" "es" "fr" "it" "ja" "ko" "la-pig" "pl" "pt" "ru" "zh-simp" "zh-trad"]
                        (keep (fn [lang]
                                (let [lang-dir (io/file dir lang)]
                                  (when (.exists lang-dir)
                                    (let [content (->> (file-seq lang-dir)
                                                       (filter #(str/ends-with? (str %) ".ftl"))
                                                       (map slurp)
                                                       (str/join "\n"))]
                                      ;; Skip empty placeholder files
                                      (when-not (str/blank? content)
                                        [lang content])))))))
             errors (volatile! [])]
         (doseq [[lang content] langs]
           (try (insert-lang! lang content)
                (catch Throwable t
                  (println "Error inserting i18n data for" lang)
                  (println (ex-message t))
                  (vswap! errors conj lang))))
         @errors))))

#?(:clj
   (comment
     (load-dictionary! "public/i18n")))

#?(:cljs
   (go (let [lang (get-in @app-state [:options :language] "en")]
         (take! (GET (str "/data/language/" lang))
                (fn [response]
                  (when (= 200 (:status response))
                    (insert-lang! lang (:json response)))))
         (when-not (= "en" lang)
           (take! (GET "/data/language/en")
                  (fn [response]
                    (when (= 200 (:status response))
                      (insert-lang! "en" (:json response)))))))))

(defn get-content
  [lang]
  (get-in @fluent-dictionary [lang :content]))

(defn get-bundle
  [lang]
  (get-in @fluent-dictionary [lang :ftl]))

(defn get-translation
  [bundle id params]
  (when bundle
    #?(:clj (fluent/format bundle id params)
       :cljs (try (fluent/format bundle id params)
                  (catch js/ReferenceError ex
                    (js/console.log "get-translation id: " id ", params: " params)
                    (js/console.log ex)
                    nil)))))

(defn format
  ([lang-cursor resource] (format lang-cursor resource nil))
  ([lang-cursor resource params]
   (let [lang (or @lang-cursor "en")
         resource (if (vector? resource) resource [resource])
         [raw-id fallback] resource
         id (name raw-id)
         bundle (get-bundle lang)
         target-translation (get-translation bundle id params)]
     (cond
       target-translation {:translation target-translation :target-language true}
       fallback {:translation fallback :target-language nil}
       :else {:translation (get-translation (get-bundle "en") id params) :target-language nil}))))

;; UI functions

(defn tr-with-info
  ([resource] (tr-with-info resource nil))
  ([resource params]
   (format language-cursor resource params)))

(defn tr
  ([resource] (tr resource nil))
  ([resource params]
   (:translation (tr-with-info resource params))))

(defn- i18n-keys
  "put params into tr-element as data if needed"
  [params]
  (into {} (map (fn [[k v]] [(keyword (str "data-i18n-param-" (name k))) (str v)])) params))

(defn- embed-content
  [translation content]
  ;; note - it's very awkward to do translations where there is an embedded element on the inside
  ;; this will allow us to do that by passing in a content map, like:
  ;; {:link [:a {:href ....}]}
  (if (or (not content) (empty? content))
    translation
    (loop [elements [translation]
           [pattern & patterns] (keys content)]
      (let [insert (get content pattern)
            reg (re-pattern (str "\\[" (name pattern) "]"))
            new-elements (mapcat #(if-not (string? %)
                                    [%] (interpose insert (str/split % reg)))
                                 elements)]
        (if-not patterns
          (into [:span] new-elements)
          (recur new-elements patterns))))))

(defn tr-element-with-embedded-content
  ([element resource content] (tr-element-with-embedded-content element resource content nil))
  ([element resource content params]
   ;; note - sometimes a nil value will be passed into a tr, or the key is computed on the frontend
   ;; and the 'else' value is nil (ie no run phase), so we need to make sure there is no black
   ;; screen in a case like this.
   (if (seq resource)
     (let [{translation :translation
            success? :target-language} (tr-with-info resource params)]
       [element
        (merge {:data-i18n-key (first resource) :data-i18n-success success?} (i18n-keys params))
        (or (embed-content translation content) "-")])
     [element {:data-i18n-failure true} "[no resource]"])))

(defn tr-element
  ([element resource] (tr-element element resource nil))
  ([element resource params] (tr-element-with-embedded-content element resource nil params)))

(defn tr-span
  ([resource] (tr-element :span resource nil))
  ([resource params] (tr-element :span resource params)))

(defn clean-input
  [s]
  (assert (seq s) "Given empty string")
  (-> (or s "")
      (str/replace " " "-")
      (str/replace "&" "-")
      (str/replace "'" "-")
      (str/replace "." "")
      (str/lower-case)))

(defn tr-fix-server-name
  [s]
  (let [cleaned (clean-input s)]
    (if-let [[_ num] (re-matches #"server-(\d+)" cleaned)]
      {:msg "server-num" :num num}
      {:msg cleaned})))

(defn tr-type [s] (tr [:card-type_name s] {:type (clean-input s)}))
(defn tr-side [s] (tr [:side_name s] {:side (clean-input s)}))
(defn tr-faction [s] (tr [:faction_name s] {:faction (clean-input s)}))
(defn tr-format [s] (tr [:format_name s] {:format (clean-input s)}))
(defn tr-sort-order [s] (tr [:sort_order_name s] {:sort (clean-input s)}))
(defn tr-room-type [s] (tr [:lobby_type s] {:type (clean-input s)}))
(defn tr-pronouns [s] (tr [:pronouns s] {:pronoun (clean-input s)}))
(defn tr-set [s]
  (let [s (if (#{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (first s))
            (str "a" s)
            s)]
    (tr [:set_name s] {:name (clean-input s)})))
(defn tr-game-prompt [s] (tr [:game_prompt s] (tr-fix-server-name s)))

(defn tr-data [k data]
  (or (get-in data [:localized k]) (k data)))

;; game log messages

(defn join-with-and
  [ms]
  (str/join (tr :join-with-and) ms))

(defn get-card-title
  [m]
  (if (:cid m) (get-title m) m))

(defn join-list
  [ms]
  (->> ms
       (mapv get-card-title)
       (str/join (tr :join-list))))

(defonce server-names (atom {}))
(reset! server-names {})

(defn format-server-name
  [server-name]
  (let [lang @language-cursor]
    (or (get-in @server-names [lang server-name])
        (let [s (name server-name)
              sn (case s
                   ("HQ" "hq") "hq"
                   ("R&D" "rd") "rd"
                   ("Archives" "archives") "archives"
                   #_:else
                   (cond
                     (str/starts-with? s "Server") (subs s 7)
                     (str/starts-with? s "remote") (str (parse-long (subs s 6)))))
              tr (tr :server-name {:server sn})]
          (-> (swap! server-names assoc-in [lang server-name] tr)
              (get-in [lang server-name]))))))

(defn format-card-str
  [card]
  (let [m (card-str-edn card)]
    (tr (:card/str m) m)))

(defn format-payment-msg
  [m]
  (tr (:payment/type m) (update-keys m name)))

(defn build-pay-msg
  [ms]
  (when-let [ms (seq ms)]
    (->> ms
         (mapv format-payment-msg)
         (join-with-and))))

(defn format-msg-args
  [m]
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
      card-str (assoc! "card-str" (format-card-str card-str))
      card-strs (assoc! "card-strs" (->> card-strs
                                         (mapv format-card-str)
                                         (str/join (tr :join-list))))
      true (persistent!))))

(defn format-effect-msgs
  [{effect-type :effect/type :as m}]
  (tr effect-type (format-msg-args m)))

(defn build-ability-msg
  [ms]
  (when-let [ms (seq ms)]
    (->> ms
         (mapv format-effect-msgs)
         (join-with-and))))

(defn build-msg
  [{msg-type :msg/type :as m}]
  (let [m (if (:msg/effect-msgs m)
            (assoc m :do-ability (build-ability-msg (:msg/effect-msgs m)))
            m)]
    (tr msg-type (format-msg-args m))))

;; definitions

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

#?(:clj
   (defmacro simple-msg
     "wraps `game.macros/effect`, calls `->use-card-msg` with each opts as effect-msg map.

     can also be given a keyword, which is wrapped as `{:effect/type opt}`."
     [& opts]
     `(game.macros/effect
       (->use-card-msg ~'card [~@opts]))))

#?(:clj
   (defmacro msg-with-cost
     "wraps `game.macros/effect`, calls `->use-card-msg` with each opts as effect-msg map.

     can also be given a keyword, which is wrapped as `{:effect/type opt}`."
     [& opts]
     `(game.macros/effect
       (->use-card-msg ~'card [~@opts] (vals (:cost-paid ~'eid))))))
