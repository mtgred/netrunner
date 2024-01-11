(ns game.rewrite
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [jinteki.utils :refer [slugify]]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn deftest? [zloc]
  (= "deftest" (z/string zloc)))

(defn testing? [zloc]
  (= "testing" (z/string zloc)))

(defn basic-test? [zloc]
  (and (testing? zloc)
       (re-matches #"(?i).*basic .*test.*" (-> zloc z/right z/string))))

(defn get-node-symbol
  "handles `(deftest X ...)` and `(deftest ^{...} X ...)
  metadata is a list and the final item in the list is the symbol"
  [zloc]
  (let [zloc (z/right zloc)]
    (if-let [zloc (z/down zloc)]
      (->> zloc
           (z/right)
           (z/string))
      (z/string zloc))))

(defn get-testing-branches [zloc]
  (->> zloc
       (z/down)
       (iterate z/right)
       (take 100)
       (remove #(basic-test? (z/down %)))
       (filter #(testing? (z/down %)))
       doall))

(defn slurp-and-spit [zloc]
  (-> zloc
      z/remove ; testing
      z/down ; re-enter list
      z/remove ; "asdf"
      z/down ; re-enter list
      z/splice ; remove outer list
      z/up))

(defn slurp-and-spit-basic-test [zloc]
  (z/replace
    zloc
    (let [zloc (z/subzip zloc)]
      (loop [zloc (z/down zloc)]
        (cond
          ;; basic test
          (-> zloc z/down basic-test?)
          (z/root (slurp-and-spit (-> zloc z/down)))
          ;; at the end?
          (not (-> zloc z/right))
          (-> zloc z/root)
          :else
          (recur (-> zloc z/right)))))))

(defn remove-testing-branches [zloc]
  (z/replace
    zloc
    (loop [zloc (z/down (z/subzip zloc))]
      (let [final-position? (not (-> zloc z/right))]
        (cond
          ;; final position testing branch
          (and final-position?
               (-> zloc z/down testing?)
               (not (-> zloc z/down basic-test?)))
          (-> zloc z/remove z/root)
          ;; final position all else
          final-position?
          (-> zloc z/root)
          ;; non-final testing branch
          (and (-> zloc z/down testing?)
               (not (-> zloc z/down basic-test?)))
          (recur (-> zloc z/remove*))
          :else
          (recur (-> zloc z/right)))))))

(defn build-deftest-name [deftest-name string]
  (symbol (str deftest-name "-" (slugify string))))

(defn prepend-deftest [deftest-name branch]
  (let [string (-> branch z/down z/right z/string)
        new-deftest-name (build-deftest-name deftest-name string)
        comment (try (n/comment-node
                       (->> string
                            (str/split-lines)
                            (str/join " ")
                            (#(subs % 1 (dec (count %))))
                            (#(str "; " % "\n"))))
                     (catch Exception _
                       (prn "exception" string)
                       "; TODO \n")
                     (catch java.lang.AssertionError _
                       (prn "assertion" string)
                       (n/comment-node "; TODO \n"))
                     )
        ]
    (-> branch
        z/down
        (z/replace 'deftest)
        z/right
        (z/replace new-deftest-name)
        z/right
        (z/insert-left comment)
        (z/insert-space-left 3)
        z/up
        z/node)))

(defn create-new-deftest [zloc deftest-name branch]
  (-> zloc
      z/insert-newline-right
      z/right*
      z/insert-newline-right
      z/right*
      (z/insert-right (prepend-deftest deftest-name branch))
      z/right))

(defn convert-tests-to-deftests [zloc testing-branches]
  (let [deftest-name (get-node-symbol (z/down zloc))]
    (loop [zloc zloc
           branches testing-branches]
      (if-let [branch (first branches)]
        (recur
          (create-new-deftest zloc deftest-name branch)
          (next branches))
        zloc))))

(defn process-deftest [zloc]
  (if (deftest? (z/down zloc))
    (let [testing-branches (get-testing-branches zloc)
          zloc (-> zloc
                   remove-testing-branches
                   slurp-and-spit-basic-test
                   (convert-tests-to-deftests testing-branches))]
      zloc)
    zloc))

(defn rewrite-file [zloc]
  (let [zloc (process-deftest zloc)]
    (if (not (z/right zloc))
      zloc
      (recur (z/right zloc)))))

(defn process-file [file]
  (->> file
       (z/of-file)
       (rewrite-file)
       (z/root-string)
       (spit file)))

(defn apply-fn-to-file [f]
  (let [dir (io/file "test/clj/game/cards")]
    (doseq [file (file-seq dir)
            :when (.isFile file)]
      (f file))))

(defn rewrite-card-tests []
  (apply-fn-to-file process-file))

(defn clean-file [zloc]
  (z/prewalk
    (z/up zloc)
    (fn [zloc]
      (and (z/list? zloc)
           (-> zloc z/down deftest?)
           (not (-> zloc z/down z/right z/right))))
    z/remove))

(defn process-file-to-clean [file]
  (->> file
       (z/of-file)
       (clean-file)
       (z/root-string)
       (spit file)))

(defn clean-card-tests []
  (apply-fn-to-file process-file-to-clean))

(defn load-test [& [path]]
  (->> (io/file (str "test/clj/game/cards/" (or path "agendas") "_test.clj"))
       (z/of-file)))
