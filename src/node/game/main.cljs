(ns game.main
  (:require [cljs.nodejs :as node]))

(aset js/exports "main" game.main)
(enable-console-print!)
(defn noop [])
(set! *main-cli-fn* noop)
