(ns hooks.cond-plus-hook
  (:require [clj-kondo.hooks-api :as api]))

(defn analyze-clauses [clauses]
  (reduce
    (fn [found-else? clause]
      ;; non-sequence clause
      (if (not (or (api/list-node? clause)
                   (api/vector-node? clause)))
        (let [{:keys [row col]} (meta clause)]
          (api/reg-finding!
            {:message "must be sequence"
             :type :cond-plus/sequence
             :row row
             :col col})
          found-else?)
        (let [[sym arrow fn-expr] (api/sexpr clause)]
          (cond
            ;; non-final else
            found-else?
            (do (api/reg-finding!
                  (merge
                    {:message ":else must be in final position"
                     :type :cond-plus/non-final-else}
                    found-else?))
              (reduced nil))
            ;; check fn-exprs
            (and (or (= :> arrow)
                     (= '=> arrow))
                 (nil? fn-expr))
            (let [{:keys [row col]} (meta clause)]
              (api/reg-finding!
                {:message "fn-expr must have third position symbol"
                 :type :cond-plus/missing-fn
                 :row row
                 :col col})
              found-else?)
            ;; else handling
            (or (= :else sym)
                (= 'else sym))
            (if found-else?
              (let [{:keys [row col]} (meta clause)]
                (api/reg-finding!
                  {:message "only one :else clause allowed"
                   :type :cond-plus/empty-else
                   :row row
                   :col col})
                ;; early exit cuz not worth analyzing the rest
                (reduced nil))
              (do (when-not arrow
                    (let [{:keys [row col]} (meta clause)]
                      (api/reg-finding!
                        {:message ":else must have a body"
                         :type :cond-plus/empty-else
                         :row row
                         :col col})))
                  ;; Store row and col from existing else as we don't throw until
                  ;; we've seen a following clause
                  (select-keys (meta clause) [:row :col])))))))
    nil
    clauses))

(defn cond+ [{:keys [node]}]
  (analyze-clauses (rest (:children node)))
  node)
