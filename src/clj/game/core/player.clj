(ns game.core.player)

(defrecord HandSize
  [base total])

(defrecord Corp
  [aid
   user
   identity
   options
   basic-action-card
   deck
   deck-id
   hand
   discard
   scored
   rfg
   play-area
   current
   set-aside
   set-aside-tracking
   servers
   properties
   click
   click-per-turn
   credit
   bad-publicity
   toast
   hand-size
   agenda-point
   agenda-point-req
   keep
   quote])

(defrecord Servers
  [hq rd archives])

(defrecord BadPublicity
  [base additional])

(defn new-corp
  [user c-identity options deck deck-id c-quote]
  (map->Corp
    {:aid 0
     :user user
     :identity c-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area [] :current [] :set-aside [] :set-aside-tracking {}
     :servers (map->Servers {:hq {:content [] :ices []}
                             :rd {:content [] :ices []}
                             :archives {:content [] :ices []}})
     :click 0 :click-per-turn 3
     :credit 5
     :bad-publicity (map->BadPublicity {:base 0 :additional 0})
     :toast []
     :properties {}
     :hand-size (map->HandSize {:base 5 :total 5})
     :agenda-point 0 :agenda-point-req 7
     :keep false
     :quote c-quote}))

(defrecord Runner
  [aid
   user
   identity
   options
   basic-action-card
   deck
   deck-id
   hand
   discard
   scored
   rfg
   play-area
   current
   set-aside
   set-aside-tracking
   rig
   toast
   click
   click-per-turn
   credit
   run-credit
   link
   tag
   properties
   memory
   hand-size
   agenda-point
   agenda-point-req
   hq-access
   rd-access
   rd-access-fn
   brain-damage
   keep
   quote])

(defrecord Rig
  [facedown hardware program resource])

(defrecord Tags
  [base total is-tagged])

(defn new-runner
  [user r-identity options deck deck-id r-quote]
  (map->Runner
    {:aid 0
     :user user
     :identity r-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area [] :current [] :set-aside [] :set-aside-tracking {}
     :rig (map->Rig {:facedown [] :hardware [] :program [] :resource []})
     :toast []
     :click 0 :click-per-turn 4
     :credit 5 :run-credit 0
     :properties {}
     :link 0
     :tag (map->Tags {:base 0 :total 0 :is-tagged false})
     :memory {:base 4
              :available 0
              :used 0
              :only-for {}}
     :hand-size (map->HandSize {:base 5 :total 5})
     :agenda-point 0 :agenda-point-req 7
     :rd-access-fn seq
     :hq-access-fn shuffle
     :brain-damage 0
     :keep false
     :quote r-quote}))
