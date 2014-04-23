{:dev {:dependencies [[weasel "0.2.0"]
                      [com.cemerick/piggieback "0.1.3"]]

       :plugins [[cider/cider-nrepl "0.1.0-SNAPSHOT"]]

       :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

       :injections [(require 'weasel.repl.websocket)
                    (defn brepl []
                      (cemerick.piggieback/cljs-repl :repl-env (weasel.repl.websocket/repl-env)))]}}
