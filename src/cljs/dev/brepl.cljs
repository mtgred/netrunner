(ns dev.brepl
  (:require [weasel.repl :as ws-repl]))

(ws-repl/connect "ws://localhost:9001" :verbose true)
(enable-console-print!)
