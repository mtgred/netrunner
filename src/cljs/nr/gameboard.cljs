(ns nr.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize includes? join lower-case split]]
            [differ.core :as differ]
            [jinteki.utils :refer [str->int]]
            [jinteki.cards :refer [all-cards]]
            [nr.appstate :refer [app-state]]
           ; [nr.auth :refer [avatar] :as auth]
           ; [nr.cardbrowser :refer [add-symbols] :as cb]
            [nr.utils :refer [toastr-options influence-dot]]
            [nr.ws :as ws]
            ))

(defn mute-spectators [mute-state]
  (ws/ws-send! [:netrunner/mute-spectators {:gameid-str (:gameid @game-state) :mute-state mute-state}]))
