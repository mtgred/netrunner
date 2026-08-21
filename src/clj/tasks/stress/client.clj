(ns tasks.stress.client
  "Headless jinteki client for stress testing. Talks to a running server the
  same way the browser does: HTTP register/login, then a real websocket over
  which it receives the state diffs and tracks game state with differ/patch."
  (:require
    [cheshire.core :as json]
    [clojure.core.async :as async]
    [clojure.string :as str]
    [differ.core :as differ]
    [org.httpkit.client :as http]
    [taoensso.sente :as sente]))

(defn- cookie-pair [set-cookie]
  (first (str/split (or set-cookie "") #";")))

(defn- scrape-csrf [body]
  (second (re-find #"data-csrf-token=\"(.*?)\"" (str body))))

(defn login!
  "Registers the user if needed and logs in.
  Returns {:cookie ... :csrf ...} for the websocket connection."
  [base-url username password]
  (let [home @(http/get base-url {:as :text})
        ring-cookie (cookie-pair (get-in home [:headers :set-cookie]))
        csrf (scrape-csrf (:body home))
        _ @(http/post (str base-url "/register")
                      {:headers {"Cookie" ring-cookie "x-csrf-token" csrf}
                       :form-params {:username username :password password
                                     :confirm-password password
                                     :email (str username "@stress.invalid")}})
        login @(http/post (str base-url "/login")
                          {:headers {"Cookie" ring-cookie "x-csrf-token" csrf}
                           :form-params {:username username :password password}})]
    (when (not= 200 (:status login))
      (throw (ex-info (str "login failed for " username
                           " (status " (:status login)
                           (when-let [err (:error login)] (str ", " err))
                           ")")
                      {:status (:status login) :body (:body login)})))
    {:cookie (str ring-cookie "; " (cookie-pair (get-in login [:headers :set-cookie])))
     :csrf csrf}))

(defn set-blocked-users!
  "Replaces the user's block list, like saving it in account settings."
  [base-url {:keys [cookie csrf]} usernames]
  (let [resp @(http/put (str base-url "/profile")
                        {:headers {"Cookie" cookie
                                   "x-csrf-token" csrf
                                   "Content-Type" "application/json"}
                         :body (json/generate-string {:blocked-users usernames})})]
    (when (not= 200 (:status resp))
      (throw (ex-info "failed to set blocked users" {:status (:status resp) :body (:body resp)})))))

(defn- clear-processed-action!
  "Like the browser client, an action counts as processed once our side's
  :aid changes (game.main/set-action-id bumps it per handled command)."
  [{:keys [game-state counters pending-action]}]
  (when-let [{:keys [side aid sent-at]} @pending-action]
    (when (not= aid (get-in @game-state [side :aid]))
      (reset! pending-action nil)
      (swap! counters update :latencies conj (/ (- (System/nanoTime) sent-at) 1e6)))))

(defn- resync! [{:keys [game-state lobby-state counters pending-action send-fn]}]
  (swap! counters update :errors inc)
  (reset! pending-action nil)
  (when-let [gameid (or (:gameid @lobby-state)
                        (some-> (:gameid @game-state) parse-uuid))]
    (send-fn [:game/resync {:gameid gameid}])))

(defn- apply-diff! [{:keys [game-state] :as client} data]
  (when @game-state
    (let [{:keys [gameid diff]} (json/parse-string data true)
          old-seq (:sequence @game-state 0)]
      (when (= gameid (:gameid @game-state))
        (try
          (swap! game-state differ/patch diff)
          ;; websockets guarantee order but not delivery; log-only diffs leave
          ;; :sequence untouched, so only a jump past old+1 means we missed one
          (when (> (:sequence @game-state 0) (inc old-seq))
            (resync! client))
          (catch Exception _
            (resync! client)))))))

(defn- handle-event [{:keys [game-state lobby-state counters] :as client} [id data]]
  (case id
    :lobby/list (swap! counters update :lobby-lists inc)
    :lobby/state (reset! lobby-state (or data {:not-in-a-lobby true}))
    :game/start (do (reset! game-state (json/parse-string data true))
                    (swap! counters update :diffs inc))
    :game/resync (reset! game-state (json/parse-string data true))
    :game/diff (do (apply-diff! client data)
                   (swap! counters update :diffs inc)
                   (clear-processed-action! client))
    :game/error (resync! client)
    nil)
  ;; only game progress counts as activity, or keepalive pings defeat the wedge watchdog
  (when (#{:game/start :game/diff :game/resync} id)
    (reset! (:last-event-at client) (System/currentTimeMillis))))

(def ^:private msgpack-connect-opts
  "Extra client opts when the classpath has the msgpack wire (sente 1.20+):
  the msgpack packer plus a binary-capable ws client. Nil against an older
  edn-wire server, whose sente client connects by itself and packs edn."
  (when (try (requiring-resolve 'taoensso.sente.packers.msgpack/get-packer)
             (catch java.io.FileNotFoundException _ nil))
    ((requiring-resolve 'tasks.stress.msgpack/connect-opts))))

(defn connect!
  "Opens a websocket as `username` and starts the receive loop.
  Returns a client map; deref :game-state / :lobby-state to observe."
  [base-url {:keys [cookie csrf]} username counters]
  (let [uri (java.net.URI. base-url)
        port (.getPort uri)
        chsk (sente/make-channel-socket-client!
              "/chsk" csrf
              (merge {:type :ws
                      :protocol (keyword (.getScheme uri))
                      :host (.getHost uri)
                      :port (when (pos? port) port)
                      :headers {"Cookie" cookie "Origin" base-url}}
                     msgpack-connect-opts))
        client {:username username
                :chsk chsk
                :game-state (atom nil)
                :lobby-state (atom nil)
                :pending-action (atom nil)
                :reconnected? (atom false)
                :last-event-at (atom (System/currentTimeMillis))
                :counters counters
                :send-fn (:send-fn chsk)}]
    ;; a reconnect means the server evicted us from our game; the slot must reset
    (add-watch (:state chsk) ::reconnect
               (fn [_ _ old new]
                 (when (and (:open? new) (not (:open? old)) (:ever-opened? old))
                   (reset! (:reconnected? client) true))))
    (async/go-loop []
      (when-let [msg (async/<! (:ch-recv chsk))]
        (try
          (handle-event client (:event msg))
          (catch Exception e
            (swap! counters update :errors inc)
            (println "client" username "event error:" (.getMessage e))))
        (recur)))
    ;; sente connects asynchronously and drops sends on a closed chsk, so
    ;; wait for the handshake before anyone gets to send
    (loop [waited 0]
      (when (and (not (:open? @(:state chsk))) (< waited 15000))
        (Thread/sleep 100)
        (recur (+ waited 100))))
    (when-not (:open? @(:state chsk))
      (sente/chsk-disconnect! (:chsk chsk))
      (throw (ex-info (str "websocket never opened for " username) {})))
    client))

(defn disconnect! [client]
  (sente/chsk-disconnect! (:chsk (:chsk client))))

(defn send-event! [client event]
  ((:send-fn client) event))

(defn send-chat!
  "Sends an in-game chat message; the server logs it and diffs it to everyone."
  [client gameid text]
  (swap! (:counters client) update :chats inc)
  (send-event! client [:game/say {:gameid gameid :msg text}]))

(defn send-action!
  "Sends a game action and locks the client until the server reflects it."
  [client side gameid command args]
  (reset! (:pending-action client) {:side side
                                    :aid (get-in @(:game-state client) [side :aid])
                                    :sent-at (System/nanoTime)})
  (swap! (:counters client) update :actions inc)
  (send-event! client [:game/action {:gameid gameid :command command :args args}]))

(defn wait-for
  "Polls f every 100ms until it returns truthy or timeout-ms passes.
  Returns the value or nil."
  [f timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (f)
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 100)
            (recur))))))
