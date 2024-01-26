(ns tasks.changed
  (:require
   [clojure.java.io :as io]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [clojure.string :as str]))

(defn changes-credits? [zloc]
  (and (z/list? zloc)
       (= "is" (-> zloc z/down z/string))
       (= "changes-credits"
          (-> zloc z/down z/right z/down z/string))))

(comment
  (changes-credits?
    (z/of-string "(is (changes-credits (get-corp) 5 (func 1)))")))

(defn changes-credits-pieces [zloc]
  (some->> zloc
           z/down
           z/right
           z/down
           (iterate z/right*)
           (take-while some?)
           (remove z/whitespace?)))

(comment
  (->> (z/of-string "(is (changes-credits (get-corp) 5 (func 1)) \"asf\")")
       z/down
       z/right
       z/right
       (z/node)))

(defn changes-zloc? [zloc]
  (and (z/list? zloc)
       (= "changes-val-macro"
          (-> zloc z/down z/string))))

(defn changes-pieces [zloc]
  (some->> zloc
           z/down
           (iterate z/right*)
           (take-while some?)
           (remove z/whitespace?)))

(defn drop-do [body]
  (reduce
    (fn [acc cur]
      (cond
        (and (z/list? cur)
               (= "do" (-> cur z/down z/string)))
        (into acc (->> cur
                       z/down
                       z/right
                       (iterate z/right*)
                       (take-while some?)
                       (remove z/whitespace?)))
        (not (z/whitespace? cur))
        (conj acc cur)
        :else acc))
    []
    body))

(comment
  (->> [(z/of-string "-1")
        (z/of-string "0")
        (z/of-string "(do 1 (do 2) 3 4)")]
       (drop-do)
       (map z/node)))

(defn gather [zloc]
  (cond
    (changes-zloc? zloc)
    (when-let [[_ amt expr msg & rest] (changes-pieces zloc)]
      [(z/node amt)
       (z/node expr)
       (drop-do rest)
       (z/node msg)])
    (changes-credits? zloc)
    (when-let [[_ side amt & rest] (changes-credits-pieces zloc)]
      (let [msg (-> zloc z/down z/right z/right)]
        [(z/node amt)
         (n/coerce (list :credit (z/node side)))
         (drop-do rest)
         (when msg (z/node msg))]))))

(defn subgather
  [init body]
  (loop
    [ctx init
     body body]
    (if (seq body)
      (let [cur (first body)]
        (if-let [[amt expr rest] (gather cur)]
          (recur (update ctx :changes
                         conj [expr amt])
                 (concat rest (next body)))
          (recur (update ctx :forms conj cur)
                 (next body))))
      ctx)))

(defn process-deftest [zloc]
  (let [[row col] (z/position zloc)]
    (-> zloc
        (z/prewalk
          (fn select [zloc]
            (or (changes-zloc? zloc)
                (changes-credits? zloc)))
          (fn visit [zloc]
            (let [[amt expr body msg] (gather zloc)
                  sub-form (subgather
                             {:forms []
                              :changes [[expr amt]]}
                             body)
                  [row' col'] (z/position zloc)
                  row'' (+ row row')
                  col'' (+ col col')
                  new-node [(n/token-node 'is)
                            (n/spaces 1)
                            (n/list-node
                              (list*
                                (n/token-node 'changed?)
                                (n/spaces 1)
                                (n/vector-node
                                  (->> (:changes sub-form)
                                       (mapcat
                                         (fn [[e a]]
                                           [e (n/spaces 1)
                                            a (n/newlines 1)
                                            (n/spaces (+ col'' 5))]))
                                       (butlast)
                                       (butlast)))
                                (n/newlines 1)
                                (->> (:forms sub-form)
                                     (mapcat
                                       #(vector
                                          (n/spaces (+ col' 5))
                                          (z/node %)
                                          (when-not
                                            (z/whitespace-or-comment? %)
                                            (n/newlines 1))))
                                     (filter some?)
                                     (butlast))))]
                  new-node (if msg
                             (conj new-node
                                   (n/newlines 1)
                                   (n/spaces (+ col' 3))
                                   msg)
                             new-node)]
              (z/replace zloc (n/list-node new-node))))))))

(defn rewrite-file [zloc]
  (let [zloc (if (= "deftest" (-> zloc z/down z/string))
               (process-deftest zloc)
               zloc)]
    (if (z/right zloc)
      (recur (z/right zloc))
      zloc)))

(defn process-file [file]
  (->> (z/of-file file {:track-position? true})
       (rewrite-file)
       (z/root-string)
       (spit file)))

(comment
  (doseq [file (sort (file-seq (io/file "test/clj/game")))
          :when (.isFile file)
          :when (not (str/includes? (str file) "macros"))]
    (process-file (str file)))
  nil)
