(ns jinteki.utils
  (:require [clojure.string :as str]))

(def INFINITY 2147483647)

(defn str->int
  [string]
  #?(:clj (java.lang.Integer/parseInt (re-find #"^\d+" string))
     :cljs (js/parseInt string 10)))

(defn side-from-str [side-str]
  (keyword (str/lower-case side-str)))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction str/lower-case (str/replace " " "-"))))

(defn other-side [side]
  (cond (= side :corp) :runner
        (= side :runner) :corp
        :else nil))

(defn count-bad-pub
  "Counts number of bad pub corp has (real + additional)"
  [state]
  (+ (get-in @state [:corp :bad-publicity :base] 0)
     (get-in @state [:corp :bad-publicity :additional] 0)))

(defn has-bad-pub?
  "Returns truthy if corp has any bad publicity"
  [state]
  (pos? (count-bad-pub state)))

(defn count-tags
  "Counts number of tags runner has (real + additional)"
  [state]
  (or (get-in @state [:runner :tag :total]) 0))

(defn count-real-tags
  "Count number of non-additional tags"
  [state]
  (or (get-in @state [:runner :tag :base]) 0))

(defn is-tagged?
  "Returns truthy if runner is tagged"
  [state]
  (or (get-in @state [:runner :tag :is-tagged])
      (pos? (count-tags state))))

(defn slugify
  "As defined here: https://you.tools/slugify/"
  ([string] (slugify string "-"))
  ([string sep]
   (if-not (string? string) ""
     (as-> string $
       #?(:clj (java.text.Normalizer/normalize $ java.text.Normalizer$Form/NFD)
          :cljs (.normalize $ "NFD"))
       (str/replace $ #"[^\x00-\x7F]+" "")
       (str/lower-case $)
       (str/trim $)
       (str/split $ #"[ \t\n\x0B\f\r!\"#$%&'()*+,-./:;<=>?@\\\[\]^_`{|}~]+")
       (filter seq $)
       (str/join sep $)))))

(defn superuser?
  [user]
  (or (:isadmin user)
      (:ismoderator user)
      (:tournament-organizer user)))

(defn capitalize [string]
  (if (pos? (count string))
    (str (str/upper-case (first string)) (subs string 1))
    ""))

(defn decapitalize [string]
  (if (pos? (count string))
    (str (str/lower-case (first string)) (subs string 1))
    ""))

(defn make-label
  "Looks into an ability for :label, if it doesn't find it, capitalizes :msg instead."
  [ability]
  (capitalize (or (:label ability)
                  (and (string? (:msg ability))
                       (:msg ability))
                  "")))

(defn add-cost-to-label
  [ability]
  (let [label (make-label ability)
        cost-label (:cost-label ability)]
    (cond
      (and (not (str/blank? cost-label))
           (not (str/blank? label)))
      (str cost-label ": " label)
      :else
      label)))

(defn select-non-nil-keys
  "Returns a map containing only those entries in map whose key is in keys and whose value is non-nil"
  [m keyseq]
  (loop [ret (transient {})
         keyseq (seq keyseq)]
    (if keyseq
      (let [k (first keyseq)
            entry (get m k ::not-found)]
        (recur
          (if (and (not= entry ::not-found)
                   (some? entry))
            (assoc! ret k entry)
            ret)
          (next keyseq)))
      (with-meta (persistent! ret) (meta m)))))

(def command-info
  [{:name "/adv-counter"
    :has-args :required
    :usage "/adv-counter n"
    :help "set advancement counters on a card to n (player's own cards only). Deprecated in favor of /counter ad n"}
   {:name "/bp"
    :has-args :required
    :usage "/bp n"
    :help "Set your bad publicity to n"}
   {:name "/bug"
    :usage "/bug"
    :help "Report a bug on GitHub"}
   {:name "/card-info"
    :usage "/card-info"
    :help "display debug info about a card (player's own cards only)"}
   {:name "/charge"
    :usage "/charge"
    :help "Charge an installed card"}
   {:name "/clear-win"
    :usage "/clear-win"
    :help "requests game to clear the current win state.  Requires both players to request it"}
   {:name "/click"
    :has-args :required
    :usage "/click n"
    :help "Set your clicks to n"}
   {:name "/close-prompt"
    :usage "/close-prompt"
    :help "close an active prompt and show the next waiting prompt, or the core click actions"}
   {:name "/counter"
    :has-args :required
    :usage "/counter n"
    :help "set counters on a card to n (player's own cards only). Attempts to infer the type of counter to place. If the inference fails, you must use the next command to specify the counter type."}
   {:name "/counter"
    :has-args :required
    :usage "/counter type n"
    :help "set the specified counter type on a card to n (player's own cards only). Type must be agenda, advance, credit, power, or virus. Can be abbreviated as ag, ad, c, p, or v respectively."}
   {:name "/credit"
    :has-args :required
    :usage "/credit n"
    :help "Set your credits to n"}
   {:name "/deck"
    :has-args :required
    :usage "/deck #n"
    :help "Put card number n from your hand on top of your deck"}
   {:name "/derez"
    :usage "/derez"
    :help "derez a rezzed card (corp only)"}
   {:name "/disable-card"
    :usage "/disable-card"
    :help "Disable a card"}
   {:name "/discard"
    :has-args :required
    :usage "/discard #n"
    :help "Discard card number n from your hand"}
   {:name "/discard-random"
    :usage "/discard-random"
    :help "Discard a random card from your hand"}
   {:name "/draw"
    :has-args :optional
    :usage "/draw n"
    :help "Draw n cards"}
   {:name "/enable-api-access"
    :usage "/enable-api-access"
    :help "Enables API access for the current game"}
   {:name "/enable-card"
    :usage "/enable-card"
    :help "Enable a card"}
   {:name "/end-run"
    :usage "/end-run"
    :help "End the run (Corp only)"}
   {:name "/error"
    :usage "/error"
    :help "Displays an error toast"}
   {:name "/facedown"
    :usage "/facedown"
    :help "Install a card facedown (Runner only)"}
   {:name "/handsize"
    :has-args :required
    :usage "/handsize n"
    :help "Set your handsize to n"}
   {:name "/host"
    :usage "/host"
    :help "Manually host a card on another card"}
   {:name "/install"
    :usage "/install"
    :help "Install an arbitrary card from hand or your discard pile"}
   {:name "/install-ice"
    :usage "/install-ice"
    :help "Install a piece of ice at any position in a server (Corp only)"}
   {:name "/install-free"
    :usage "/install-free"
    :help "Install an arbitrary card from hand or your discard pile, ignoring all costs"}
   {:name "/jack-out"
    :usage "/jack-out"
    :help "Jack out (Runner only)"}
   {:name "/link"
    :has-args :required
    :usage "/link n"
    :help "Set your link to n"}
   {:name "/mark"
    :usage "/mark"
    :help "Identify your mark"}
   {:name "/memory"
    :has-args :required
    :usage "/memory n"
    :help "Set your memory to n"}
   {:name "/move-bottom"
    :usage "/move-bottom"
    :help "Pick a card in your hand to put on the bottom of your deck"}
   {:name "/move-deck"
    :usage "/move-deck"
    :help "Pick a card from your play-area to put on top of your deck"}
   {:name "/move-hand"
    :usage "/move-hand"
    :help "Pick a card from your play-area to put into your hand"}
   {:name "/peek"
    :has-args :optional
    :usage "/peek n"
    :help "See n top cards of your deck"}
   {:name "/psi"
    :usage "/psi"
    :help "Start a Psi game (Corp only)"}
   {:name "/reload-id"
    :usage "/reload-id"
    :help "Reloads your ID (this can sometimes fix gamestates)"}
   {:name "/replace-id"
    :has-args :required
    :usage "/replace-id n"
    :help "Replace your ID with the card \"n\""}
   {:name "/rez"
    :usage "/rez"
    :help "Choose a card to rez, ignoring all costs (Corp only)"}
   {:name "/rez-all"
    :usage "/rez-all"
    :help "Rez all cards, ignoring all costs and flip cards in archives faceup (Corp only). For revealing your servers at the end of a game."}
   {:name "/rez-free"
    :usage "/rez-free"
    :help "Choose a card to rez, ignoring all costs and on-rez abilities (Corp only)"}
   {:name "/rfg"
    :usage "/rfg"
    :help "Choose a card to remove from the game"}
   {:name "/roll"
    :has-args :required
    :usage "/roll n"
    :help "Roll an n-sided die"}
   {:name "/sabotage"
    :has-args :required
    :usage "/sabotage n"
    :help "Sabotage n cards"}
   {:name "/save-replay"
    :usage "/save-replay"
    :help "Save a replay of the game"}
   {:name "/score"
    :usage "/score"
    :help "Score an agenda from hand or from the board, ignoring all restrictions (corp only)"}
   {:name "/set-mark"
    :has-args :required
    :usage "/set-mark n"
    :help "Set the central server n as your mark (Runner only)"}
   {:name "/show-hand"
    :usage "/show-hand"
    :help "Shows your hand in the chat log (does not proc reveal triggers)"}
   {:name "/summon"
    :has-args :required
    :usage "/summon n"
    :help "Add card \"n\" to your hand (from outside the game)"}
   {:name "/swap-ice"
    :usage "/swap-ice"
    :help "Swap the position of 2 installed pieces of ice (Corp only)"}
   {:name "/swap-installed"
    :usage "/swap-installed"
    :help "Swap the position of two installed non-ice (Corp only)"}
   {:name "/tag"
    :has-args :required
    :usage "/tag n"
    :help "Set your tags to n"}
   {:name "/take-core"
    :has-args :required
    :usage "/take-core n"
    :help "Take n core damage (Runner only)"}
   {:name "/take-meat"
    :has-args :required
    :usage "/take-meat n"
    :help "Take n meat damage (Runner only)"}
   {:name "/take-net"
    :has-args :required
    :usage "/take-net n"
    :help "Take n net damage (Runner only)"}
   {:name "/trace"
    :has-args :required
    :usage "/trace n"
    :help "Start a trace with base strength n (Corp only)"}
   {:name "/trash"
    :usage "/trash"
    :help "Trash an installed card"}
   {:name "/undo-click"
    :usage "/undo-click"
    :help "Resets the game back to start of the click.  One click only retained. Only allowed for active player"}
   {:name "/undo-turn"
    :usage "/undo-turn"
    :help "Resets the game back to end of the last turn. Requires both players to request it"}
   {:name "/unique"
    :usage "/unique"
    :help "Toggles uniqueness of selected card (can be used to e.g. play with non-errata version of Wireless Net Pavillion)"}])
