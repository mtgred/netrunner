(ns prod.nr
  (:require
    [nr.main :as main]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(main/init!)
