(ns netrunner.toast)

(def toastr-options (js-obj
                     "closeButton" false
                     "debug" false
                     "newestOnTop" false
                     "progressBar" false
                     "positionClass" "toast-card"
                     "preventDuplicates" true
                     "onclick" nil
                     "showDuration" 300
                     "hideDuration" 3000
                     "timeOut" 3000
                     "extendedTimeOut" 1000
                     "showEasing" "swing"
                     "hideEasing" "linear"
                     "showMethod" "fadeIn"
                     "hideMethod" "fadeOut"))

(declare toast)

(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type]
  (set! (.-options js/toastr) toastr-options)
  (let [f (aget js/toastr type)] 
    (f msg)))
