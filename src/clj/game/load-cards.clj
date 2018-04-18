(in-ns 'game.core)

(load-file "src/clj/game/cards/utils.clj")

;; Adapted from Stack Overflow: https://stackoverflow.com/a/20708175/3023252
(let [all-files (->> "src/clj/game/cards"
                     (java.io.File.)
                     (file-seq)
                     (sort))]
  (doseq [dirs (filter #(.isDirectory %) all-files)]
    (doseq [file (file-seq dirs)]
      (when (.isFile file)
        (load-file (.getCanonicalPath file))))))
