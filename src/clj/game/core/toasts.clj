(ns game.core.toasts)

(defn toast
  "Adds a message to toast with specified severity (default as a warning) to the toast message list.
  If message is nil, removes first toast in the list.
  For options see http://codeseven.github.io/toastr/demo.html
  Currently implemented options:
    - msg-type (warning, info etc)
    - time-out (sets both timeOut and extendedTimeOut currently)
    - close-button
    - prevent-duplicates"
  ([state side message] (toast state side message "warning" nil))
  ([state side message msg-type] (toast state side message msg-type nil))
  ([state side message msg-type options]
   ;; Allows passing just the toast msg-type as the options parameter
   (when message
     ;; normal toast - add to list
     (swap! state update-in [side :toast] #(conj % {:msg message :type msg-type :options options :id (random-uuid)})))))

(defn ack-toast
  ([state side {:keys [id]}]
   (when-let [id (when (string? id) (parse-uuid id))]
     (swap! state update-in [side :toast] (fn [toasts] (remove #(= (:id %) id) toasts))))))

(defn show-error-toast
  [state side]
  (when state
    (toast state side
           (str "Your last action caused a game error on the server. You can keep playing, but there "
                "may be errors in the game's current state. Please click the button below to submit a report "
                "to our GitHub issues page.<br/><br/>Use /error to see this message again.")
           "exception"
           {:time-out 0 :close-button true})))
