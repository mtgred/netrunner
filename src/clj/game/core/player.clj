(ns game.core.player)

(defrecord HandSize
  [base mod])

(defrecord Corp
  [user
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
   servers
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
    {:user user
     :identity c-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area []
     :servers (map->Servers {:hq {} :rd {} :archives {}})
     :click 0 :click-per-turn 3
     :credit 5
     :bad-publicity (map->BadPublicity {:base 0 :additional 0})
     :toast []
     :hand-size (map->HandSize {:base 5 :mod 0})
     :agenda-point 0 :agenda-point-req 7
     :keep false
     :quote c-quote}))

(defrecord Runner
  [user
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
   rig
   toast
   click
   click-per-turn
   credit
   run-credit
   link
   tag
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
  [program resource hardware])

(defrecord Tags
  [base additional is-tagged])

(defrecord Memory
  [base mod used])

(defn new-runner
  [user r-identity options deck deck-id r-quote]
  (map->Runner
    {:user user
     :identity r-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area []
     :rig (map->Rig {:program [] :resource [] :hardware []})
     :toast []
     :click 0 :click-per-turn 4
     :credit 5 :run-credit 0
     :link 0
     :tag (map->Tags {:base 0 :additional 0 :is-tagged 0})
     :memory (map->Memory {:base 4 :mod 0 :used 0})
     :hand-size (map->HandSize {:base 5 :mod 0})
     :agenda-point 0 :agenda-point-req 7
     :hq-access 1 :rd-access 1
     :rd-access-fn seq
     :brain-damage 0
     :keep false
     :quote r-quote}))
