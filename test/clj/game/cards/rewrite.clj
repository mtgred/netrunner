(ns game.cards.rewrite
  (:require
   [clojure.java.io :as io]
   [jinteki.utils :refer [slugify]]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn load-agendas []
  (->> (io/file "test/clj/game/cards/agendas_test.clj")
       (z/of-file)))

(defn deftest? [zloc]
  (= "deftest" (z/string zloc)))

(defn testing? [zloc]
  (= "testing" (z/string zloc)))

(defn basic-test? [zloc]
  (and (testing? zloc)
       (re-matches #"(?i).*basic.*" (-> zloc z/right z/string))))

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
          (boolean (-> zloc z/right z/end?))
          (-> zloc z/root)
          :else
          (recur (-> zloc z/right)))))))

(defn remove-testing-branches [zloc]
  (z/replace
    zloc
    (loop [zloc (z/down (z/subzip zloc))]
      (let [final-position? (boolean (-> zloc z/right z/end?))]
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
        new-deftest-name (build-deftest-name deftest-name string)]
    (-> branch
        z/down
        (z/replace 'deftest)
        z/right
        (z/replace new-deftest-name)
        z/right
        (z/insert-left (n/comment-node (str "; " (subs string 1 (dec (count string))) "\n")))
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

(defn process-file [zloc]
  (let [zloc (process-deftest zloc)]
    (if (z/end? (z/right zloc))
      zloc
      (recur (z/right zloc)))))

(comment
  (def agendas (load-agendas))
  (do (process-file agendas) nil))
