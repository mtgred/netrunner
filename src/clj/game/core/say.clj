(ns game.core.say
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [game.core.card :refer [get-title]]
   [game.core.schemas :as schemas]
   [game.core.toasts :refer [toast]]
   [jinteki.cards :refer [all-cards]]
   [malli.core :as m]
   [noahtheduke.fluent :as fluent]))

(defn make-message
  "Create a message map, along with timestamp if none is provided."
  [{:keys [user text timestamp]
    :or {timestamp (inst/now)}}]
  {:user (if (= "__system__" user) user (select-keys user [:username :emailhash]))
   :text (if (string? text) (str/trim text) text)
   :timestamp timestamp})

(defn make-system-message
  "Creates a message map from the __system__ user, which won't display a username."
  [text]
  (make-message {:user "__system__" :text text}))

(defn- select-pronoun
  "Selects an appropriate plurular pronoun
  'their' is neuter, so it's appropriate to everyone as a fallback"
  [user]
  (let [key (get-in user [:options :pronouns])]
    (case key
      "they" "their"
      "she" "her"
      "sheit" "its"
      "shethey" "their"
      "he" "his"
      "heit" "its"
      "hethey" "their"
      "heshe" "their"
      "heshe2" "her"
      "it" "its"
      "faefaer" "faer"
      "ne" "nir"
      "ve" "vis"
      "ey" "eir"
      "zehir" "hir"
      "zezir" "zir"
      "xe" "xyr"
      "xi" "xir"
      "their")))

(defn- insert-pronouns
  "inserts pronouns into text based on the side speaking"
  [state side text]
  (let [corp-pronoun (select-pronoun (get-in @state [:corp :user]))
        runner-pronoun (select-pronoun (get-in @state [:runner :user]))
        user-pronoun (cond
                       (= side :corp) corp-pronoun
                       (= side :runner) runner-pronoun
                       :else "their")]
    (when text
      (-> text
          (str/replace #"(\[pronoun\])|(\[their\])" user-pronoun)
          (str/replace #"\[corp-pronoun\]" corp-pronoun)
          (str/replace #"\[runner-pronoun\]" runner-pronoun)))))

(defn- log
  [state message]
  (swap! state update :log conj message))

(defn say
  "Prints a message to the log as coming from the given user."
  ([state side {:keys [user text]} {:keys [log-side]
                                    :or {log-side :public}}]
   (let [author (or user (get-in @state [side :user]))
         message (make-message {:user author
                                :text (if (string? text)
                                        (insert-pronouns state side text)
                                        (update text :raw-text #(insert-pronouns state side %)))})
         log-sides (if (vector? log-side) log-side [log-side])]
     (log state (zipmap log-sides (repeat message))))))

(defn system-say
  "Prints a system message to log (`say` from user __system__)"
  ([state side text] (system-say state side text nil))
  ([state side text {:keys [hr log-side] :as args}]
   (say state side (make-system-message (merge {:username nil}
                                               (if (string? text) {:raw-text text} text)))
        args)
   (when hr (say state side (make-system-message {:raw-text "[hr]"}) args))))

(defn unsafe-say
  "Prints a reagent hiccup directly to the log. Do not use for any user-generated content!"
  [state text]
  (let [message (make-system-message text)]
    (log state {:public message})))

(defonce store (atom #{}))

(comment
  (reset! store #{}))

(def card-names
  (->> (keys @all-cards)
       (sort)
       (reverse)
       (str/join "|")
       (#(str "(" % ")"))
       (re-pattern)))

(def type-names
  (->> (vals @all-cards)
       (map :type)
       (sort)
       (reverse)
       (str/join "|")
       (#(str "(" % ")"))
       (re-pattern)))

(comment
  (->> @store
       (filter #(str/includes? % "and draw")))

  (->> @store
       (map #(-> %

                 ;; names
                 (str/replace "realloc()" "{$card}")
                 (str/replace "Ghost Runner" "{$card}")
                 (str/replace "Masterwork (v37)" "{$card}")
                 (str/replace "Gemilang Arena: Burning Bright" "{$card}")
                 (str/replace "AirbladeX (JSRF Ed.)" "{$card}")
                 (str/replace #"Dewi Subrotoputri: (Pedagogical Dhalang|Shadow Guide)" "{\\$card}")
                 (str/replace #"Hoshiko Shiro: (Untold Protagonist|Mahou Shoujo)" "{\\$card}")
                 (str/replace #"(Subsurface Labs|Tenure Floors|Disposal Grounds): Méliès U" "{\\$card}")
                 (str/replace #"[Cc]orp  ?" "{\\$side} ")
                 (str/replace #"[Rr]unner  ?" "{\\$side} ")
                 (str/replace card-names "{\\$card}")
                 (str/replace type-names "{\\$type}")
                 (str/replace #"\{\$type\}s" "{\\$type}")
                 (str/replace #"(the )?[Gg]rip" "the grip")
                 (str/replace #"(the )?[Ss]tack" "the stack")
                 (str/replace #"(the )?[Hh]eap" "the heap")
                 (str/replace #"(Archives|HQ|R&D)" "{\\$central}")
                 (str/replace #"Server \d+" "{\\$server}")

                 ;; common subtypes
                 (str/replace #"(Code Gate|Barrier|Sentry|Harmonic)" "{\\$subtype}")

                 ;; contains numbers
                 (str/replace #"position \d+" "{\\$position}")
                 (str/replace #"turn \d+" "{\\$turn}")
                 (str/replace #"sabotage \d+" "{\\$sabotage}")
                 (str/replace #"guess \d+" "{\\$guess}")
                 (str/replace #"(-|\+)?\d+-cost card" "{\\$target-cost-card}")

                 ;; quantities
                 (str/replace #"\d+ +\[[Cc]redits?\]" "{\\$credits}")
                 (str/replace #" all +\[[Cc]redits?\]" " {\\$all-credits}")
                 (str/replace #"\d+ *\[[Rr]ecurring [Cc]redits?\]" "{\\$recurring-credits}")
                 (str/replace #"(-|\+)?\d+ fewer (\[[Cc]licks?\])+" "{\\$fewer-clicks}")
                 (str/replace #"(-|\+)?\d+ allotted (\[[Cc]licks?\])+" "{\\$allotted-clicks}")
                 (str/replace #" all +(\[[Cc]licks?\])+" " {\\$all-clicks}")
                 (str/replace #" all +remaining +(\[[Cc]licks?\])+" " {\\$all-remaining-clicks}")
                 (str/replace #" ((\d+|a) )?(\[[Cc]licks?\])+" " {\\$clicks}")
                 (str/replace #"\d+ +(\[tags?\])+" "{\\$tags}")
                 (str/replace #" (\d+|a) +cards?" " {\\$cards}")
                 (str/replace #"\d+ +(\[cards?\])+" "{\\$cards}")
                 (str/replace #"\d+ +random +(\[cards?\])+" "{\\$random-cards}")
                 (str/replace #"\d+ +random +cards?" "{\\$random-cards}")
                 (str/replace #"\d+ +(copy|copies)" "{\\$copies}")
                 (str/replace #" all +(\[cards?\])+" " {\\$all-cards}")
                 (str/replace #"\d+ +virus counters?" "{\\$virus-counters}")
                 (str/replace #"\d+ +(meat) damage" "{\\$meat-damages}")
                 (str/replace #"\d+ +(net) damage" "{\\$net-damages}")
                 (str/replace #"\d+ +(brain|core) damage" "{\\$core-damages}")
                 (str/replace #"\d+ +tags?" "{\\$tags}")
                 (str/replace #" all +tags?" " {\\$all-tags}")
                 (str/replace #"\d+ +turns?" "{\\$turns}")
                 (str/replace #"\d+ +extra turns?" "{\\$extra-turns}")
                 (str/replace #" (\d+|a) +additional turns?" " {\\$additional-turns}")
                 (str/replace #" all( +\d+)? +subroutines?" " {\\$all-subroutines}")
                 (str/replace #"\d+ +subroutines?" "{\\$subroutines}")
                 (str/replace #"\d+ +unbroken +subroutines? on \{\$card\} \(.*?\)" "{\\$unbroken-subroutines} on {\\$card} ({\\$subs})")
                 (str/replace #"\d+ +\{\$subtype\} ice" "{\\$subtype} {\\$ices}")
                 (str/replace #"\d+ +\{\$subtype\} subroutines?" "{\\$subtype} {\\$subroutines}")
                 (str/replace #"\d+ +bad publicity" "{\\$bad-publicity}")
                 (str/replace #" all +bad publicity" " {\\$all-bad-publicity}")
                 (str/replace #" (\d+|an) additional +bad publicity" " {\\$additional-bad-publicity}")
                 (str/replace #"\d +installed cards?" "{\\$installed-cards}")
                 (str/replace #"\d +installed programs?" "{\\$installed-programs}")
                 (str/replace #" (-|\+)?(\d+|an) +agenda( points?)?" " {\\$agendas}")
                 (str/replace #" \{\$agendas\} worth \{\$agendas\}" " {\\$type} worth {\\$agenda-points}")
                 (str/replace #" an \{\$type\} worth \d+ points" " an agenda worth {\\$agenda-points}")
                 (str/replace #" ((\d+|a) )?pieces? of ice" " {\\$ices}")
                 (str/replace #" (\d|an) +installed non-ice" " {\\$installed-non-ice}")
                 (str/replace #" (\d+|an) +advancement counters?" " {\\$advancement-counters}")
                 (str/replace #" (\d+|an) +additional advancement counters?" " {\\$advancement-counters}")
                 (str/replace #" (\d+|a) +hosted advancement counters?" " {\\$hosted-advancement-counters}")
                 (str/replace #" all +hosted advancement counters?" " {\\$all-hosted-advancement-counters}")
                 (str/replace #" (\d+|a) +agenda counters?" " {\\$agenda-counters}")
                 (str/replace #" all +agenda counters?" " {\\$all-agenda-counters}")
                 (str/replace #" (\d+|a) +hosted agenda counters?" " {\\$hosted-agenda-counters}")
                 (str/replace #" all +hosted agenda counters?" " {\\$all-hosted-agenda-counters}")
                 (str/replace #" (\d+|a) +charge counters?" " {\\$charge-counters}")
                 (str/replace #" (\d+|a) +power counters?" " {\\$power-counters}")
                 (str/replace #" (\d+|a) +hosted power counters?" " {\\$hosted-power-counters}")
                 (str/replace #" all +hosted power counters?" " {\\$all-hosted-power-counters}")
                 (str/replace #" (\d+|a) +hosted virus counters?" " {\\$virus-counters}")
                 (str/replace #" all virus counters?" " {\\$all-virus-counters}")
                 (str/replace #" (\d+|an) +additional cards?" " {\\$additional-cards}")
                 (str/replace #" (\d|a) +counter" " {\\$counters}")

                 ;; phrases
                 (str/replace #"(for|at) no cost" "at no cost")
                 (str/replace #"(-|\+)?\d+ strength" "{\\$str}")
                 (str/replace #"give \{\$card\} (-|\+)?\d+ strength" "give {\\$card} {\\$str}")
                 (str/replace #"link strength to \d+" "link strength to {\\$link-str}")
                 (str/replace #"trash( the)? top (cards?|\{\$cards?\})( (of|from) (\{\$central\}|the stack))?"
                              "trash the top {\\$cards} from {\\$deck}")
                 (str/replace #"trashes \d+ installed rezzed ice" "trashes {\\$installed-rezzed-ices}")
                 (str/replace #"start a psi game \(.*?\)" "start a psi game ({\\$ability-text})")
                 (str/replace #"increase( its)? strength from \d+ to \d+" "increase its strength from {\\$start-str} to {\\$end-str}")
                 (str/replace #"increase( the)? strength of \{\$card\} to \d+" "increase the strength of {\\$card} to {\\$end-str}")
                 (str/replace #"increase( the)? strength of \{\$card\} from \d+ to \d+" "increase the strength of {\\$card} from {\\$start-str} to {\\$end-str}")

                 (str/replace #"\{\$card\}(,?( and)? \{\$card\})+" "{\\$enumerated-cards}")

                 ;; subroutines
                 (str/replace #"to break( \d+)? \{\$subroutines\} on \{\$card\} \((.*?)\)\." "to break {\\$subroutines} on {\\$card} ({\\$subroutine-text}).")
                 (str/replace #"to break( \d+)? \{\$subtype\} \{\$subroutines\} on \{\$card\} \((.*?)\)\." "to break {\\$subtype} {\\$subroutines} on {\\$card} ({\\$subroutine-text}).")
                 (str/replace #"to resolve the subroutine \((.*?)\) from" "to resolve the subroutine ({\\$subroutine-text}) from")

                 ;; traces
                 (str/replace #"initiate a trace with strength \d+ \((.*)\)" "initiate a trace with {\\$trace-strength} ({\\$trace-text})")
                 (str/replace #"initiate a trace with strength \d+" "initiate a trace with {\\$trace-strength}")
                 (str/replace #"increase trace strength to \d+" "increase trace strength to {\\$trace-strength}")

                 ))
       (sort)
       (distinct)
       (vec)
       (pprint/pprint)))

(defn ^:private prep-msg
  [state side text]
  (let [username (get-in @state [side :user :username])]
    (merge {:username username
            :side side}
      (if (string? text)
        {:raw-text (str username " " text ".")}
        text))))

(defn system-msg
  "Prints a message to the log without a username."
  ([state side text] (system-msg state side text nil))
  ([state side text args]
   (let [msg (prep-msg state side text)]
     (system-say state side msg args))))

(defn multi-msg
  [state side message-map]
  (doseq [[log-side text] message-map
          :let [message (prep-msg state side text)]]
    (system-say state side message {:log-side log-side})))

(defn enforce-msg
  "Prints a message related to a rules enforcement on a given card.
  Example: 'Architect cannot be trashed while installed.'"
  [state card text]
  (system-say state nil (str (:title card) " " text ".")))

(defn implementation-msg
  [state card]
  (when (not= :full (:implementation card))
    (system-say state nil (str "[!] " (:title card) " - " (:implementation card)))))

(defn indicate-action
  [state side _]
  (system-say state side
              (str "[!] Please pause, " (if (= side :corp) "Corp" "Runner") " is acting."))
  (toast state side
         "You have indicated action to your opponent"
         "info"
         {:time-out 2000 :close-button false})
  (toast state (if (= side :corp) :runner :corp)
         "Pause please, opponent is acting"
         "info"
         {:time-out 5000 :close-button true}))

(defn play-sfx
  "Adds a sound effect to play to the sfx queue.
  Each SFX comes with a unique ID, so each client can track for themselves which sounds have already been played.
  The sfx queue has size limited to 3 to limit the sound torrent tabbed out or lagged players will experience."
  [state _ sfx]
  (swap! state (fn [state]
                 (if-let [current-id (:sfx-current-id state)]
                   (-> state
                       (update :sfx conj {:id (inc current-id) :name sfx})
                       (update :sfx #(take 3 %))
                       (update :sfx-current-id inc))
                   state))))

(defn n-last-logs
  "Gets the n last log messages not sent by a user (ie game logs only)"
  ([state n] (n-last-logs state n :public))
  ([state n side]
   (if @state
     ;; this should filter out user-typed messages, so we don't accidentally
     ;; spy on private conversations
     (->> @state :log (keep side)
          (filter #(= (:user %) "__system__"))
          (map :text)
          (take-last n)
          (str/join "\n\t"))
     "unable to fetch log from state")))
