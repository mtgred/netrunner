{:dev {:dependencies [[weasel "0.4.0-SNAPSHOT"]
                      [com.cemerick/piggieback "0.1.3"]
                      [org.bodil/cljs-noderepl "0.1.11"]]

       :plugins [[cider/cider-nrepl "0.7.0"]]

       :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

       :injections [(require 'weasel.repl.websocket)
                    (require '[cljs.repl.node :as node])
                    (defn brepl []
                      (cemerick.piggieback/cljs-repl :repl-env (weasel.repl.websocket/repl-env)))
                    (defn node-repl []
                      (node/run-node-nrepl))]}}
