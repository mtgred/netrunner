(ns nr.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljc.java-time.local-date :as ld]
   [cljs.core.async :refer [<! chan put!] :as async]
   [clojure.string :as s]
   [jinteki.cards :refer [all-cards] :as cards]
   [jinteki.utils :refer [slugify str->int]]
   [medley.core :refer [find-first]]
   [nr.account :refer [alt-art-name]]
   [nr.ajax :refer [GET]]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr tr-faction tr-format tr-side tr-sort tr-type tr-data]]
   [nr.utils :refer [banned-span deck-points-card-span faction-icon
                     format->slug get-image-path image-or-face influence-dots
                     non-game-toast render-icons restricted-span rotated-span set-scroll-top slug->format
                     store-scroll-top]]
   [reagent.core :as r]))

(defonce cards-channel (chan))

(declare generate-previous-cards)
(declare generate-flip-cards)
(declare insert-starter-info)
(declare insert-starter-ids)
(declare merge-localized-data)

(defn- format-card-key->string
  [format]
  (assoc format :cards
                (reduce-kv
                  (fn [m k v] (assoc m (name k) v))
                  {} (:cards format))))

(go (let [server-version (get-in (<! (GET "/data/cards/version")) [:json :version])
          lang (get-in @app-state [:options :language] "en")
          local-cards (js->clj (.parse js/JSON (.getItem js/localStorage "cards")) :keywordize-keys true)
          need-update? (or (not local-cards)
                           (not= server-version (:version local-cards))
                           (not= lang (:lang local-cards)))
          latest-cards (if need-update?
                           (:json (<! (GET "/data/cards")))
                           (:cards local-cards))
          localized-data (if (not= lang "en")
                           (:json (<! (GET (str "/data/cards/lang/" lang)))))
          cards (->> latest-cards
                     (insert-starter-ids)
                     (merge-localized-data localized-data)
                     (sort-by :code))
          sets (:json (<! (GET "/data/sets")))
          cycles (:json (<! (GET "/data/cycles")))
          mwls (:json (<! (GET "/data/mwl")))
          latest-mwl (->> mwls
                          (map (fn [e] (update e :date-start ld/parse)))
                          (group-by #(keyword (:format %)))
                          (mapv (fn [[k, v]] [k (->> v
                                                     (sort-by :date-start)
                                                     (last)
                                                     (format-card-key->string))]))
                          (into {}))
          alt-info (->> (<! (GET "/data/cards/altarts"))
                        (:json)
                        (map #(select-keys % [:version :name :description :artist-blurb :artist-link :artist-about])))]
      (reset! cards/mwl latest-mwl)
      (reset! cards/sets sets)
      (reset! cards/cycles cycles)
      (swap! app-state assoc :sets sets :cycles cycles)
      (when need-update?
        (.setItem js/localStorage "cards" (.stringify js/JSON (clj->js {:cards cards :version server-version :lang lang}))))
      (reset! all-cards (into {} (map (juxt :title identity) (sort-by :code cards))))
      (swap! app-state assoc
             :cards-loaded true
             :all-cards-and-flips (merge @all-cards (generate-flip-cards cards))
             :previous-cards (generate-previous-cards cards)
             :alt-info alt-info)
      (put! cards-channel cards)))

(defn- merge-localized-data
  [localized-data cards]
  (let [localized-data-indexed (into {} (map (juxt :code identity) localized-data))]
    (map #(assoc % :localized (dissoc (localized-data-indexed (:code %)) :code))
         cards)))

(defn- insert-starter-info
  [card]
  (-> card
      (assoc :influencelimit "∞")
      (assoc-in [:format :standard] {:banned true})
      (assoc-in [:format :startup] {:banned true})
      (assoc-in [:format :sunset] {:banned true})
      (assoc-in [:format :eternal] {:banned true})
      (assoc-in [:format :snapshot] {:banned true})
      (assoc-in [:format :snapshot-plus] {:banned true})
      (assoc-in [:format :neo] {:banned true})))

(defn- insert-starter-ids
  "Add special case info for the Starter Deck IDs"
  [cards]
  (->> cards
       (map #(if (= (:title %) "The Catalyst: Convention Breaker") (insert-starter-info %) %))
       (map #(if (= (:title %) "The Syndicate: Profit over Principle") (insert-starter-info %) %))))

(defn- expand-face [card acc f]
  (let [flip (f (:flips card))
        updated (-> card
                    (assoc :title (:title flip)
                           :text (:text flip)
                           :images (:images (f (:faces card))))
                    (dissoc :faces :flips))]
    (conj acc updated)))

(defn- expand-one-flip [acc card]
  (let [faces (keys (:flips card))]
    (reduce (partial expand-face card) acc faces)))

(defn- generate-flip-cards [cards]
  (let [flips (filter :flips cards)
        modified (reduce expand-one-flip [] flips)]
    (into {} (map (juxt :title identity) (sort-by :code modified)))))

(defn- keys-in [m]
  (if (map? m)
    (vec
      (mapcat (fn [[k v]]
                (let [sub (keys-in v)
                      nested (map #(into [k] %) (filter (comp not empty?) sub))]
                  (if (seq nested)
                    nested
                    [[k]])))
              m))
    []))

(defn- update-nested-images
  [code images acc nested-key]
  (if (= (keyword code) (last nested-key))
    (let [value (get-in images nested-key)
          new-key (conj (pop nested-key) :stock)]
      (assoc-in acc new-key value))
    acc))

(defn- update-previous-image-paths
  [prev]
  (let [code (:code prev)
        images (:images prev)
        nested-keys (keys-in images)]
    (reduce (partial update-nested-images code images) {} nested-keys)))

(defn- expand-one
  "Reducer function to create a previous card from a newer card definition."
  [acc {:keys [code set_code]} c]
  (let [number (str->int (subs code 3))
        prev-set (find-first #(= set_code (:code %)) @cards/sets)
        prev (-> c
                 (assoc
                   :code code
                   :rotated true
                   :cycle_code (:cycle_code prev-set)
                   :setname (:name prev-set)
                   :set_code (:id prev-set)
                   :number number
                   :future-version (:code c))
                 (dissoc :previous-versions))
        prev (assoc prev :images (update-previous-image-paths prev))]
    (conj acc prev)))

(defn- expand-previous
  "Reducer function to expand a card with :previous-cards defined"
  [acc c]
  (reduce #(expand-one %1 %2 c) acc (:previous-versions c)))

(defn generate-previous-cards
  "The cards database only has the latest version of a card. Create stubs for previous versions of a card for display purposes."
  [cards]
  (let [c (filter :previous-versions cards)]
    (reduce expand-previous [] c)))

(defn show-alt-art?
  "Is the current user allowed to use alternate art cards and do they want to see them?"
  ([] (show-alt-art? false))
  ([allow-all-users]
   (and (get-in @app-state [:options :show-alt-art] true)
        (or allow-all-users
            (get-in @app-state [:user :special] false)))))

(defn image-url
  ([card] (image-url card false))
  ([card allow-all-users]
   (let [lang (get-in @app-state [:options :language] "en")
         res (get-in @app-state [:options :card-resolution] "default")
         art (if (show-alt-art? allow-all-users)
               (get-in @app-state [:options :alt-arts (keyword (:code card))] "stock")
               "stock")
         images (image-or-face card)]
     (get-image-path images (keyword lang) (keyword res) (keyword art)))))

(defn- base-image-url
  "The default card image. Displays an alternate image if the card is specified as one."
  [card]
   (let [lang (get-in @app-state [:options :language] "en")
         res (get-in @app-state [:options :card-resolution] "default")
         art (if (keyword? (:art card)) (:art card) :stock)]
     (get-image-path (:images card) (keyword lang) (keyword res) art)))

(defn- alt-version-from-string
  "Given a string name, get the keyword version or nil"
  [setname]
  (when-let [alt (find-first #(= setname (:name %)) (:alt-info @app-state))]
    (keyword (:version alt))))

(defn- expand-alts
  [only-version acc card]
   (let [lang (get-in @app-state [:options :language] "en")
         res (get-in @app-state [:options :card-resolution] "default")
         alt-versions (remove #{:prev} (map keyword (map :version (:alt-info @app-state))))
         images (select-keys (merge (get-in (:images card) [(keyword lang) :default])
                                    (get-in (:images card) [(keyword lang) (keyword res)]))
                             alt-versions)
         alt-only (alt-version-from-string only-version)
         filtered-images (cond
                           (= :prev alt-only) nil
                           alt-only (list alt-only)
                           :else (keys images))]
     (if (and filtered-images
              (show-alt-art? true))
       (->> filtered-images
            (concat [""])
            (map #(if % (assoc card :art %) card))
            (map #(if (not= "" (:art %)) (dissoc % :previous-versions) %))
            (concat acc))
       (conj acc card))))

(defn- insert-alt-arts
  "Add copies of alt art cards to the list of cards. If `only-version` is nil, all alt versions will be added."
  [only-version cards]
  (reduce (partial expand-alts only-version) () (reverse cards)))

(defn- expand-flips
  [acc card]
  (if-let [faces (:faces card)]
    (->> (keys faces)
         (map #(assoc card :images (get-in card [:faces % :images])))
         (map #(dissoc % :faces))
         (concat acc))
    (conj acc card)))

(defn- insert-flip-arts
  "Add copies of cards that have multiple faces (eg. Hoshiko Shiro: Untold Protagonist)"
  [cards]
  (reduce expand-flips () (reverse cards)))

(defn- post-response [response]
  (if (= 200 (:status response))
    (let [new-alts (get-in response [:json :altarts] {})]
      (swap! app-state assoc-in [:user :options :alt-arts] new-alts)
      (non-game-toast (tr [:card-browser.update-success "Updated Art"]) "success" nil))
    (non-game-toast (tr [:card-browser.update-failure "Failed to Update Art"]) "error" nil)))

(defn- future-selected-alt-art [card]
  (let [future-code (keyword (:future-version card))
        selected-alts (:alt-arts (:options @app-state))
        selected-art (get selected-alts future-code)]
    (= (:code card) selected-art)))

(defn- previous-selected-alt-art [card]
  (let [selected-alts (:alt-arts (:options @app-state))
        selected-art (get selected-alts (keyword (:code card)))]
    (nil? selected-art)))

(defn- selected-alt-art [card]
  (cond (contains? card :future-version) (future-selected-alt-art card)
        (contains? card :previous-versions) (previous-selected-alt-art card)
        :else
        (let [code (keyword (:code card))
              selected-alts (:alt-arts (:options @app-state))
              selected-art (keyword (get selected-alts code))
              card-art (:art card)]
          (or (and card-art (nil? selected-art) (= "" card-art))
              (and selected-art (= card-art selected-art))))))

;; Alts can only be set on th most recent version of a card
;; So if the card has a :future-version key, we apply the alt to
;; that card, setting the alt to the code of the old card.
(defn- select-alt-art [card]
  (let [is-old-card (contains? card :future-version)
        art (:art card)
        code-kw (keyword (:future-version card (:code card)))
        alts (:alt-arts (:options @app-state))
        new-alts (cond
                   (keyword? art) (assoc alts code-kw (name art))
                   is-old-card (assoc alts code-kw (:code card))
                   :else (dissoc alts code-kw))] ; remove the key entirely if the newest card is selected
    (swap! app-state assoc-in [:options :alt-arts] new-alts)
    (nr.account/post-options "/profile" (partial post-response))))

(defn- text-class-for-status
  [status]
  (cond (:legal status) "legal"
        (:rotated status) "casual"
        (:banned status) "invalid"))

(defn card-as-text
  "Generate text html representation a card"
  [card show-extra-info]
  (let [title (tr-data :title card)
        icon (faction-icon (:faction card) title)
        uniq (when (:uniqueness card) "◇ ")]
    [:div
     [:h4 uniq title icon
      (when-let [influence (:factioncost card)]
        (when-let [faction (:faction card)]
           [:span.influence
            {:class (slugify faction)}
            (influence-dots influence)]))]
     (when-let [memory (:memoryunits card)]
       (if (< memory 3)
         [:div.anr-icon {:class (str "mu" memory)} ""]
         [:div.heading (str (tr [:card-browser.memory "Memory"]) ": " memory) [:span.anr-icon.mu]]))
     (when-let [cost (:cost card)]
       [:div.heading (str (tr [:card-browser.cost "Cost"]) ": " cost)])
     (when-let [trash-cost (:trash card)]
       [:div.heading (str (tr [:card-browser.trash-cost "Trash cost"]) ": " trash-cost)])
     (when-let [strength (:strength card)]
       [:div.heading (str (tr [:card-browser.strength "Strength"]) ": " strength)])
     (when-let [requirement (:advancementcost card)]
       [:div.heading (str (tr [:card-browser.advancement "Advancement requirement"]) ": " requirement)])
     (when-let [agenda-point (:agendapoints card)]
       [:div.heading (str (tr [:card-browser.agenda-points "Agenda points"]) ": " agenda-point)])
     (when-let [min-deck-size (:minimumdecksize card)]
       [:div.heading (str (tr [:card-browser.min-deck "Minimum deck size"]) ": " min-deck-size)])
     (when-let [influence-limit (:influencelimit card)]
       [:div.heading (str (tr [:card-browser.inf-limit "Influence limit"]) ": " influence-limit)])

     [:div.text.card-body
      [:p [:span.type (tr-type (:type card))]
       (if (empty? (:subtype card)) "" (str ": " (:subtype card)))]
      [:pre (render-icons (tr-data :text (get @all-cards (:title card))))]

      (when show-extra-info
        [:<>
         [:div.formats
          (doall (for [[k name] (-> slug->format butlast)]
                   (let [status (get-in card [:format (keyword k)] "unknown")
                         c (text-class-for-status status)]
                     ^{:key k}
                     [:div.format-item {:class c} (tr-format name)
                      (cond (:banned status) banned-span
                            (:restricted status) restricted-span
                            (:rotated status) rotated-span
                            (:points status) (deck-points-card-span (:points status)))])))]

         [:div.pack
          (when-let [pack (:setname card)]
            (when-let [number (:number card)]
              (str pack " " number
                   (when-let [art (:art card)]
                     (str " [" (alt-art-name art) "]")))))]
         (when (show-alt-art?)
           (if (selected-alt-art card)
             [:div.selected-alt (tr [:card-browser.selected-art "Selected Alt Art"])]
             (when (or (:art card) (:previous-versions card) (:future-version card))
               [:button.alt-art-selector
                {:on-click #(select-alt-art card)}
                (tr [:card-browser.select-art "Select Art"])])))])]]))

(defn types [side]
  (let [runner-types ["Identity" "Program" "Hardware" "Resource" "Event"]
        corp-types ["Agenda" "Asset" "ICE" "Operation" "Upgrade"]]
    (case side
      "All" (concat runner-types corp-types)
      "Runner" runner-types
      "Corp" (cons "Identity" corp-types))))

(defn factions [side]
  (let [runner-factions ["Anarch" "Criminal" "Shaper" "Adam" "Apex" "Sunny Lebeau"]
        corp-factions ["Jinteki" "Haas-Bioroid" "NBN" "Weyland Consortium" "Neutral"]]
    (case side
      "All" (concat runner-factions corp-factions)
      "Any Side" (concat runner-factions corp-factions)
      "Runner" (conj runner-factions "Neutral")
      "Corp" corp-factions
      (concat runner-factions corp-factions))))

(defn- filter-alt-art-cards [cards]
  (let [lang (get-in @app-state [:options :language] "en")
        res (get-in @app-state [:options :card-resolution] "default")]
    (filter #(or (not-empty (dissoc (get-in (:images %) [(keyword lang) (keyword res)]) :stock))
                 (contains? % :future-version)
                 (contains? % :previous-versions))
            cards)))

(defn- filter-alt-art-set [setname cards]
  (when-let [alt-key (alt-version-from-string setname)]
    (if (= alt-key :prev)
      (filter #(or (contains? % :future-version) (contains? % :previous-versions)) cards)
      (let [lang (get-in @app-state [:options :language] "en")
            res (get-in @app-state [:options :card-resolution] "default")]
        (filter #(get-in (:images %) [(keyword lang) (keyword res) alt-key]) cards)))))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (get % field) filter-value) cards)))

(defn filter-format [fmt cards]
  (if (= "All" fmt)
    cards
    (let [fmt (keyword (get format->slug fmt))]
      (filter #(get-in % [:format fmt :legal]) cards))))

(defn filter-title [query cards]
  (if (empty? query)
    cards
    (let [lcquery (s/lower-case query)]
      (filter #(or (s/includes? (s/lower-case (:title %)) lcquery)
                   (s/includes? (s/lower-case (tr-data :title %)) lcquery)
                   (s/includes? (:normalizedtitle %) lcquery))
              cards))))

(defn sort-field [fieldname]
  (case fieldname
    "Name" :title
    "Influence" (juxt :factioncost :side :faction :title)
    "Cost" (juxt :cost :title)
    "Faction" (juxt :side :faction :title)
    "Type" (juxt :side :type :faction :title)
    "Set number" :number))

(defn selected-set-name [state]
  (-> (:set-filter @state)
      (s/replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
      (s/replace " Cycle" "")))

(defn handle-scroll [_ state]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.prop $cardlist "clientHeight"))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (swap! state update-in [:page] (fnil inc 0)))))

(defn- card-view [_ _]
  (let [cv (r/atom {:show-text false})]
    (fn [card state]
      [:div.card-preview.blue-shade
       {:on-click #(do (.preventDefault %)
                       (if (= card (:selected-card @state))
                         (swap! state dissoc :selected-card)
                         (swap! state assoc :selected-card card)))
        :class (if (:decorate-card @state)
                 (cond (= (:selected-card @state) card) "selected"
                       (and (show-alt-art?) (selected-alt-art card)) "selected-alt")
                 nil)}
       (if (or (= card (:selected-card @state))
               (:show-text @cv))
         [card-as-text card true]
         (when-let [url (base-image-url card)]
           [:img {:src url
                  :alt (tr-data :title card)
                  :onError #(-> (swap! cv assoc :show-text true))
                  :onLoad #(-> % .-target js/$ .show)}]))])))

(defn card-list-view [_ scroll-top]
  (r/create-class
    {
     :display-name "card-list-view"
     :component-did-mount #(set-scroll-top % @scroll-top)
     :component-will-unmount #(store-scroll-top % scroll-top)
     :reagent-render
     (fn [state _]
       (let [selected (selected-set-name state)
             selected-cycle (slugify selected)
             combined-cards (concat (sort-by :code (vals @all-cards)) (:previous-cards @app-state))
             [alt-filter cards] (cond
                                  (= selected "All") [nil combined-cards]
                                  (= selected "Alt Art") [nil (filter-alt-art-cards combined-cards)]
                                  (s/ends-with? (:set-filter @state) " Cycle") [nil (filter #(= (:cycle_code %) selected-cycle) combined-cards)]
                                  (not (some #(= selected (:name %)) (:sets @app-state))) [selected (filter-alt-art-set selected combined-cards)]
                                  :else
                                  [nil (filter #(= (:setname %) selected) combined-cards)])
             cards (->> cards
                        (filter-cards (:side-filter @state) :side)
                        (filter-cards (:faction-filter @state) :faction)
                        (filter-cards (:type-filter @state) :type)
                        (filter-format (:format-filter @state))
                        (filter-title (:search-query @state))
                        (insert-flip-arts)
                        (insert-alt-arts alt-filter)
                        (sort-by (sort-field (:sort-field @state)))
                        (take (* (:page @state) 28)))]
         [:div.card-list {:on-scroll #(handle-scroll % state)}
          (doall
            (for [card cards]
              ^{:key (str (base-image-url card) "-" (:code card))}
              [card-view card state]))]))}))

(defn handle-search [e state]
  (doseq [filter [:set-filter :type-filter :faction-filter]]
    (swap! state assoc filter "All"))
  (swap! state assoc :sort-field "Faction")
  (swap! state assoc :search-query (.. e -target -value)))

(defn query-builder [state]
  (let [query (:search-query @state)]
    [:div.search-box
     [:span.e.search-icon {:dangerouslySetInnerHTML #js {:__html "&#xe822;"}}]
     (when-not (empty? query)
       [:span.e.search-clear {:dangerouslySetInnerHTML #js {:__html "&#xe819;"}
                              :on-click #(swap! state assoc :search-query "")}])
     [:input.search {:on-change #(handle-search % state)
                     :type "text"
                     :placeholder (tr [:card-browser.search-hint "Search cards"])
                     :value query}]]))

(defn sort-by-builder [state]
  [:div
   [:h4 (tr [:card-browser.sort "Sort by"])]
   [:select {:value (:sort-field @state)
             :on-change #(swap! state assoc :sort-field (.. % -target -value))}
    (doall
      (for [field ["Faction" "Name" "Type" "Influence" "Cost" "Set number"]]
        [:option {:value field
                  :key field
                  :dangerouslySetInnerHTML #js {:__html (tr-sort field)}}]))]])

(defn simple-filter-builder
  [title state state-key options translator]
  [:div
   [:h4 title]
   [:select {:value (get @state state-key)
             :on-change #(swap! state assoc state-key (.. % -target -value))}
    (doall
      (for [option (cons "All" options)]
        ^{:key option}
        [:option {:value option
                  :key option
                  :dangerouslySetInnerHTML #js {:__html (translator option)}}]))]])

(defn format-set-name [pack-name]
  (str "&nbsp;&nbsp;&nbsp;&nbsp;" pack-name))


(defn dropdown-builder
  [state]
  (let [sets (r/cursor app-state [:sets])
        cycles (r/cursor app-state [:cycles])
        cycles-list-all (map #(assoc % :name (str (:name %) " Cycle")
                                     :cycle_position (:position %)
                                     :position 0)
                             @cycles)
        cycles-list (filter #(not (= (:size %) 1)) cycles-list-all)
        sets-list (map #(if (not (or (:bigbox %)
                                     (= (:id %) (:cycle_code %))))
                          (update-in % [:name] format-set-name)
                          %)
                       @sets)
        set-names (map :name
                       (sort-by (juxt :cycle_position :position)
                                (concat cycles-list sets-list)))
        alt-art-sets (cons "Alt Art"
                           (map #(format-set-name (:name %))
                                (sort-by :position (:alt-info @app-state))))
        sets-to-display (if (show-alt-art? true)
                          (concat set-names alt-art-sets)
                          set-names)
        formats (-> format->slug keys butlast)]
    [:div
     (doall
       (for [[title state-key options translator]
             [[(tr [:card-browser.format "Format"]) :format-filter formats tr-format]
              [(tr [:card-browser.set "Set"]) :set-filter sets-to-display identity]
              [(tr [:card-browser.side "Side"]) :side-filter ["Corp" "Runner"] tr-side]
              [(tr [:card-browser.faction "Faction"]) :faction-filter (factions (:side-filter @state)) tr-faction]
              [(tr [:card-browser.type "Type"]) :type-filter (types (:side-filter @state)) tr-type]]]
         ^{:key title}
         [simple-filter-builder title state state-key options translator]))]))

(defn clear-filters [state]
  [:p [:button
       {:key "clear-filters"
        :on-click #(swap! state assoc
                          :search-query ""
                          :sort-field "Faction"
                          :format-filter "All"
                          :set-filter "All"
                          :type-filter "All"
                          :side-filter "All"
                          :faction-filter "All")}
       (tr [:card-browser.clear "Clear"])]])

(defn art-info [state]
  (let [selected (r/cursor state [:selected-card])]
    (when (and @selected (:art @selected))
      (let [art (name (:art @selected))
            alts (:alt-info @app-state)
            info (first (filter #(= (:version %) art) alts))
            blurb (:artist-blurb info)
            link (:artist-link info)]
        (when blurb
          [:div.panel.green-shade.artist-blurb
           [:h4 "Artist Info"]
           [:div blurb]
           (when link
             [:a {:href link} "More Info"])])))))

(defn card-browser []
  (let [state (r/atom {:search-query ""
                       :sort-field "Faction"
                       :format-filter "All"
                       :set-filter "All"
                       :type-filter "All"
                       :side-filter "All"
                       :faction-filter "All"
                       :page 1
                       :decorate-card true
                       :selected-card nil})
        scroll-top (atom 0)]

    (fn []
      [:div#cardbrowser.cardbrowser
       [:div.cardbrowser-bg]
       [:div.card-info
        [:div.blue-shade.panel.filters
         [query-builder state]
         [sort-by-builder state]
         [dropdown-builder state]
         [clear-filters state]]
        [art-info state]]
       [card-list-view state scroll-top]])))
