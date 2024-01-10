(ns tasks.changed
  (:require
   [clojure.java.io :as io]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [clojure.string :as str]))

(defn changes-zloc? [zloc]
  (and (z/sexpr-able? zloc)
       (list? (z/sexpr zloc))
       (= "changes-val-macro"
          (-> zloc z/down z/string))))

(defn changes-pieces [zloc]
  (some->> zloc
           z/down
           (iterate z/right*)
           (take-while some?)
           (remove z/whitespace?)))

(defn subgather
  [init body]
  (loop
    [ctx init
     body body]
    (if (seq body)
      (let [cur (first body)]
        (if (changes-zloc? cur)
          (let [[_ amt expr _msg & rest]
                (changes-pieces cur)]
            (recur (update ctx :changes
                           conj [(z/node expr) (z/node amt)])
                   (concat rest (next body))))
          (recur (update ctx :forms conj cur)
                 (next body))))
      ctx)))

(defn process-deftest [zloc]
  (if (= "deftest" (-> zloc z/down z/string))
    (let [[row col] (z/position zloc)]
      (-> zloc
          (z/prewalk
            (fn select [zloc]
              (changes-zloc? zloc))
            (fn visit [zloc]
              (let [[_changes amt expr msg & body]
                    (changes-pieces zloc)
                    sub-form (subgather
                               {:forms []
                                :changes [[(z/node expr) (z/node amt)]]}
                               body)
                    [row' col'] (z/position zloc)
                    row'' (+ row row')
                    col'' (+ col col')]
                (z/subedit->
                    zloc
                    (z/replace
                      (n/list-node
                        [(n/token-node 'is)
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
                                  (butlast))))
                         (n/newlines 1)
                         (n/spaces (+ col' 3))
                         (z/node msg)]))))))))
    zloc))

(defn rewrite-file [zloc]
  (let [zloc (process-deftest zloc)]
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
