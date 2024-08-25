(ns game.core.state)

(defrecord State
  [;; angel-arena-info
   active-player
   bonus
   click-state
   corp
   corp-phase-12
   effects
   effect-completed
   eid
   encounters
   end-run
   end-time
   end-turn
   events
   format
   gameid
   history
   log
   loser
   losing-deck-id
   losing-user
   mark
   options
   per-run
   per-turn
   psi
   queued-events
   reason
   rid
   room
   run
   runner
   runner-phase-12
   sfx
   sfx-current-id
   stack
   start-date
   stats
   trace
   trash
   turn
   turn-events
   turn-state
   typing
   winner
   winning-deck-id
   winning-user])

(defn make-rid
  "Returns a progressively-increasing integer to identify a new remote server."
  [state]
  (let [current-rid (:rid @state)]
    (swap! state update :rid inc)
    current-rid))

(defn new-state
  [gameid room fmt now options corp runner]
  (map->State
    {:gameid gameid
     :log []
     :active-player :runner
     :end-turn true
     :format (keyword fmt)
     :history []
     :mark nil
     :room room
     :rid 1 :turn 0 :eid 0
     :sfx [] :sfx-current-id 0
     :stats {:time {:started now}}
     :start-date now
     :options options
     :encounters []
     :corp corp
     :runner runner}))
