(ns jinteki.cards
  (:require
   #?@(:clj [[cljc.java-time.local-date :as ld]
             [integrant.core :as ig]
             [monger.collection :as mc]])
   #?@(:cljs [[reagent.core :as r]])))

(defonce all-cards #?(:clj (atom {})
                      :cljs (r/atom {})))

(defonce mwl #?(:clj (atom {})
                :cljs (r/atom {})))

(defonce sets (atom []))

(defonce cycles (atom []))

#?(:clj
    (defn- format-card-key->string
      [fmt]
      (let [cards (->> (:cards fmt)
                    (reduce-kv
                      (fn [m k v]
                        (assoc! m (name k) v))
                      (transient {}))
                    (persistent!))]
        (assoc fmt :cards cards))))

#?(:clj
    (defmethod ig/init-key :jinteki/cards [_ {{:keys [db]} :mongo}]
      (let [cards (mc/find-maps db "cards" nil)
            stripped-cards (mapv #(update % :_id str) cards)
            cards-by-title (into {} (map (juxt :title identity)) stripped-cards)
            saved-sets (mc/find-maps db "sets" nil)
            saved-cycles (mc/find-maps db "cycles" nil)
            saved-mwl (mc/find-maps db "mwls" nil)
            latest-mwl (->> saved-mwl
                         (map (fn [e] (update e :date-start ld/parse)))
                         (group-by #(keyword (:format %)))
                         (mapv (fn [[k v]] [k (->> v
                                                (sort-by :date-start)
                                                (last)
                                                (format-card-key->string))]))
                         (into {}))]
        (reset! all-cards cards-by-title)
        (reset! sets saved-sets)
        (reset! cycles saved-cycles)
        (reset! mwl latest-mwl)
        {:all-cards cards-by-title
         :sets saved-sets
         :cycles saved-cycles
         :mwl latest-mwl})))

#?(:clj
    (defmethod ig/halt-key! :jinteki/cards [_ _]
      (reset! all-cards nil)
      (reset! sets nil)
      (reset! cycles nil)
      (reset! mwl nil)))
