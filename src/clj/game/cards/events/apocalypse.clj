(in-ns 'game.cards.events)

(def card-definition-apocalypse
  {"Apocalypse"
   (let [corp-trash {:async true
                     :effect (req (let [ai (all-installed state :corp)
                                        onhost (filter #(= '(:onhost) (:zone %)) ai)
                                        unhosted (->> ai
                                                     (remove #(= '(:onhost) (:zone %)))
                                                     (sort-by #(vec (:zone %)))
                                                     (reverse))
                                        allcorp (concat onhost unhosted)]
                                    (trash-cards state :runner eid allcorp)))}
         runner-facedown {:effect (req (let [installedcards (all-active-installed state :runner)
                                             ishosted (fn [c] (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone))))
                                             hostedcards (filter ishosted installedcards)
                                             nonhostedcards (remove ishosted installedcards)]
                                         (doseq [oc hostedcards :let [c (get-card state oc)]]
                                           (flip-facedown state side c))
                                         (doseq [oc nonhostedcards :let [c (get-card state oc)]]
                                           (flip-facedown state side c))))}]
     {:req (req (and (some #{:hq} (:successful-run runner-reg))
                     (some #{:rd} (:successful-run runner-reg))
                     (some #{:archives} (:successful-run runner-reg))))
      :async true
      ;; trash cards from right to left
      ;; otherwise, auto-killing servers would move the cards to the next server
      ;; so they could no longer be trashed in the same loop
      :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
      :effect (req (wait-for
                     (resolve-ability state side corp-trash card nil)
                     (continue-ability state side runner-facedown card nil)))})})
