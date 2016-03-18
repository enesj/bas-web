(ns jus.jus
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [reagent.core :as r :refer [atom render-component]]
            [jus.helper :as h]
            [ajax.core :refer [GET POST json-response-format json-request-format url-request-format ajax-request]]
            [goog.events :as events]
            [reforms.reagent :include-macros true :as f]
            [re-com.core :as re-com :refer [h-box v-box box gap line row-button label checkbox horizontal-bar-tabs vertical-bar-tabs title p scroller single-dropdown button alert-box
                                            v-split h-split modal-panel]
             :refer-macros [handler-fn]]
            [jus.md-circle-icon-button :refer [icons example-icons]]
            [jus.utils :refer [panel-title title2 args-table material-design-hyperlink github-hyperlink status-text]]
            [re-com.popover :refer [popover-tooltip]]
            [jus.data-table :refer [data-table table-state nova-naredba-atom colors delete-dialog-template add-nova-naredba-dialog-template
                                    cljs-ajax-upload-file edit-nova-naredba-dialog-template jus-data path]])
  (:import [goog.events EventType]))




(def jus-all-data (atom {:data nil}))

(def count-veze (atom nil))

(def showing (atom {:hover? false :choice nil :prefix "A"}))

(defn new-id [] (inc (js/parseInt (:JUSId (apply max-key #(js/parseInt (:JUSId %)) (filter #(> (:Naredba %) 0) (:data @jus-data)))))))

(defn all-childs [id]
  (let [veza (:veza @table-state)
        first-level-childs (mapv :Child (filterv #(= id (:Parent %)) veza))]
    (loop [childs first-level-childs
           all-childs []]
      (if (not-empty childs)
        (recur (mapv :Child (set (flatten (doall (for [id childs] (filterv #(= id (:Parent %)) veza)))))) (into (set all-childs) childs))
        all-childs))))

(defn is-child? [id child] (some #{child} (all-childs id)))


(defn next-level [level]
  (keyword (str (inc (js/parseInt (name level))))))

(defn prev-level [level]
  (keyword (str (dec (js/parseInt (name level))))))

(defn count-veze-fn []
  (GET "/jus/count-veze" {:handler       #(reset! count-veze %)
                          :error-handler #(js/alert (str "error: " %))}))

(defn init-jus-data []
  (GET "/jus/active-data" {:handler       #(do (swap! jus-data assoc-in [:data] (first %)) (swap! table-state assoc-in [:veza] (second %)))
                           :error-handler #(js/alert (str "error: " %))}))

(defn init-only-jus []
  (GET "/jus/only-jus" {:handler       #(swap! jus-all-data assoc-in [:data] %)
                        :error-handler #(js/alert (str "error: " %))}))

(defn alow-new-veza []
  (let [path @path]
    (if (not-empty path)
      (let [lock (:Locked (first (filterv #(= (:JUSId %) (val (last path))) (:data @jus-data))))]
        (if (= lock 0) true false)))))

(defn init-veza [& opt]
  (if opt
    (let [[parent child] (first opt)]
      (swap! table-state update-in [:veza] #(merge % {:Parent parent :Child child})))
    (GET "/jus/veza" {:handler       #(swap! table-state assoc-in [:veza] %)
                      :error-handler #(js/alert (str "error: " %))})))

(defn current-parent-child [& id]
  (let [local-state @table-state
        choice (:choice @showing)
        path @path
        child (if id (first id) choice)
        parent (or ((clojure.set/map-invert (:path local-state)) child) ((keyword (str (count path))) path))]
    [parent child]))


(defn lock-jus [row]
  (let [id (:id row)
        criteria {:JUSId id}
        data (if (= 1 (:ok row)) 0 1)
        parent ((:level row) @path)]
    (GET "/jus/update" {:params        {:filter criteria :field-data [{:Locked data}] :like false}
                        :handler       (fn [x] (do (if (= x 1) (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= id (:JUSId %)) :Locked] data y))))
                                                   (swap! count-veze update-in [parent :locked]
                                                          (fn [x] (if (= 0 data) (dec x) (inc x))))))
                        :error-handler #(js/alert (str "error: " %))})))

(defn fali-jus [row]
  (let [id (:id row)
        criteria {:JUSId id}
        data (if (= 1 (:fali row)) 0 1)
        parent ((:level row) @path)]
    (GET "/jus/update" {:params        {:filter criteria :field-data [{:Fali data}] :like false}
                        :handler       (fn [x] (if (= x 1) (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= id (:JUSId %)) :Fali] data y))))
                                         )
                        :error-handler #(js/alert (str "error: " %))})))

(defn obavezan-jus [row data]
  (let [id (:id row)
        criteria {:JUSId id}]
    (GET "/jus/update" {:params        {:filter criteria :field-data [{:Mandatory data}] :like false}
                        :handler       (fn [x] (if (= x 1) (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= id (:JUSId %)) :Mandatory] data y)))))
                        :error-handler #(js/alert (str "error: " %))})))




(defn exist-veza [& id]
  (let [[parent child] (if id (current-parent-child (first id)) (current-parent-child))]
    (or (not-empty (filterv #(= % {:Parent parent :Child child}) (:veza @table-state)))
        (= parent child)
        (is-child? child parent)
        (some #{child} (vals @path))
        )))

(defn reset-path [row]
  (reset! path (into (sorted-map-by #(< (js/parseInt (name %1)) (js/parseInt (name %2)))) (take-while #(not= (next-level (:level row)) (key %)) @path))))

(defn add-veza [& id]
  (let [jus-id (if id (first id) (:choice @showing))
        [parent child] (current-parent-child jus-id)
        new (first (filterv #(= jus-id (:JUSId %)) (:data @jus-all-data)))]
    (when parent
      (init-veza [parent child])
      (GET "/jus/add-veza" {:params        {:parent parent :child child}
                            :handler       #(swap! count-veze update-in [parent]
                                                   (fn [x] {:total (inc (:total x)) :locked (+ (:locked x) (:Locked new)) :childs (conj (:childs x) child)}))
                            :error-handler #(js/alert (str "error: " %))})
      (if-not id (do (swap! showing assoc-in [:choice] nil)
                     (if-not (some #(= (:JUSId %) (:JUSId new)) (:data @jus-data))
                       (swap! jus-data update-in [:data] #(merge % new))))))))



(defn delete-veza []
  (let [local-state @table-state
        {:keys [level child]} (:delete-modal local-state)
        old (first (filterv #(= child (:JUSId %)) (:data @jus-data)))]
    (do (GET "/jus/del-veza" {:params        {:parent level :child child}
                              :handler       #(init-veza)
                              :error-handler #(js/alert (str "error: " %))})
        (swap! count-veze update-in [level]
               (fn [x] {:total (dec (:total x)) :locked (- (:locked x) (:Locked old)) :childs (remove #{child} (:childs x))}))
        (reset-path {:level ((clojure.set/map-invert @path) level)}))))

(defn delete-dialog []
  (let [process-ok (fn [event]
                     (delete-veza)
                     (swap! table-state assoc-in [:delete-modal] {:show? false :level nil :child nil}))
        process-cancel (fn [event] (swap! table-state assoc-in [:delete-modal] {:show? false :level nil :child nil}))]
    (fn [] (when (:show? (:delete-modal @table-state)) [modal-panel
                                                        :backdrop-color "grey"
                                                        :backdrop-opacity 0.4
                                                        :style {:font-family "Consolas"}
                                                        :child [delete-dialog-template
                                                                "Želite li obrisati ovu vezu?"
                                                                process-ok
                                                                process-cancel]]))))

(defn delete-jus []
  (GET "/jus/delete" {:params        {:jusid (:jusid (:delete-jus-modal @table-state))}
                      :handler       #(init-jus-data)
                      :error-handler #(js/alert (str "error: " %))}))

(defn delete-jus-dialog-template [process-ok process-cancel]
  [h-box
   :gap "12px"
   :children [[title :level :level3 :style {:font-weight "bold" :align-self "center"} :label "Želite li obrisati ovu naredbu?"]
              [button
               :label "Briši"
               :class "btn-primary"
               :on-click process-ok]
              [button
               :label "Cancel"
               :on-click process-cancel]]])


(defn delete-jus-dialog []
  (let [process-ok (fn [event]
                     (delete-jus)
                     (swap! table-state assoc-in [:delete-jus-modal] {:show? false :jsuid nil}))
        process-cancel (fn [event]
                         (swap! table-state assoc-in [:delete-jus-modal] {:show? false :jsuid nil}))]
    (fn [] (when (:show? (:delete-jus-modal @table-state)) [modal-panel :backdrop-color "grey"
                                                            :wrap-nicely? true
                                                            :backdrop-opacity 0.4
                                                            :style {:font-family "Consolas"}
                                                            :child [delete-jus-dialog-template
                                                                    process-ok
                                                                    process-cancel]]))))

(defn naredba-exist [naslov]
  (let [n-exist (first (filter #(= naslov (:JUSopis %)) (:data @jus-data)))]
    (if n-exist (:JUSId n-exist) nil)))

(defn add-nova-naredba [& old-id]
  (let [data @nova-naredba-atom
        naslov {:JUSopis (:naslov data)}
        glasnik {:Glasnik (:glasnik data)}
        direktiva {:Direktiva (:direktiva data)}
        link {:Link-d (:link data)}
        X-CSRF-Token (:X-CSRF-Token data)
        file {:Link-n (cljs-ajax-upload-file "upload-file" X-CSRF-Token)}
        naredba {:Naredba (:naredba data)}
        napomena {:Napomena (:napomena data)}
        ok {:Locked (:ok data)}
        old-id (naredba-exist (:naslov data))
        id (or old-id (new-id))
        fields-data (merge naslov direktiva link file naredba ok napomena glasnik {:JUSId id :Locked 0})]
    (if (and (> (:naredba data) 1) (not (exist-veza id))) (add-veza id))
    (if-not old-id
      (GET "/jus/insert" {:params        {:field-data [fields-data]}
                          :handler       #(init-jus-data)
                          :error-handler #(js/alert (str "error: " %))}))))


(defn add-nova-naredba-dialog []
  (let [process-ok (fn [event id]
                     (add-nova-naredba id)
                     (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :X-CSRF-Token nil})
                     (swap! table-state assoc-in [:nova-naredba-modal] {:show? false}))
        process-cancel (fn [event]
                         (reset! nova-naredba-atom {:show? false :naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :X-CSRF-Token nil})
                         (swap! table-state assoc-in [:nova-naredba-modal] {:show? false}))]
    (fn [] (when (:show? (:nova-naredba-modal @table-state)) [modal-panel :backdrop-color "grey"
                                                              :wrap-nicely? true
                                                              :backdrop-opacity 0.4
                                                              :style {:font-family "Consolas"}
                                                              :child [add-nova-naredba-dialog-template
                                                                      process-ok
                                                                      process-cancel]]))))

(defn edit-nova-naredba []
  (let [data @nova-naredba-atom
        naredba {:Naredba (:naredba data)}
        jusid {:JUSId (:jusid data)}
        naslov {:JUSopis (:naslov data)}
        glasnik {:Glasnik (:glasnik data)}
        direktiva {:Direktiva (:direktiva data)}
        link {:Link-d (:link data)}
        ok {:Locked (:ok data)}
        napomena {:Napomena (:napomena data)}
        X-CSRF-Token (:X-CSRF-Token data)
        file {:Link-n (or (cljs-ajax-upload-file "upload-file" X-CSRF-Token) (:file data))}
        fields-data (merge naslov direktiva link file glasnik naredba ok napomena)]
    (GET "/jus/update" {:params        {:filter jusid :field-data [fields-data] :like false}
                        :handler       (fn [x] (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= (:jusid data) (:JUSId %))] (merge fields-data jusid) y))))
                        :error-handler #(js/alert (str "error: " %))})))

(defn edit-nova-naredba-dialog []
  (let [process-ok (fn [event]
                     (edit-nova-naredba)
                     (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :napomena nil :X-CSRF-Token nil})
                     (swap! table-state assoc-in [:nova-naredba-modal] {:edit? false}))
        process-cancel (fn [event]
                         (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :napomena nil :X-CSRF-Token nil})
                         (swap! table-state assoc-in [:nova-naredba-modal] {:edit? false}))]
    (fn [] (when (:edit? (:nova-naredba-modal @table-state)) [modal-panel :backdrop-color "grey"
                                                              :wrap-nicely? true
                                                              :backdrop-opacity 0.4
                                                              :style {:font-family "Consolas"}
                                                              :child [edit-nova-naredba-dialog-template
                                                                      process-ok
                                                                      process-cancel]]))))

(defn add-nova-naredba-event [naredba]
  (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba naredba :ok nil :napomena nil :X-CSRF-Token (h/get-value "__anti-forgery-token")})
  (swap! table-state assoc-in [:nova-naredba-modal] {:show? true}))

(defn edit-nova-naredba-event [_ data]
  (reset! nova-naredba-atom {:jusid (:id data) :naslov (:naslov-full data) :glasnik (:glasnik data) :direktiva (:direktiva data)
                             :link  (:link-d data) :file (:link-n data) :naredba (:naredba data) :ok (:ok data) :napomena (:napomena data) :X-CSRF-Token (h/get-value "__anti-forgery-token")})
  (swap! table-state assoc-in [:nova-naredba-modal] {:edit? true}))

(defn delete-veza-event [level row]
  (swap! table-state assoc-in [:delete-modal] {:show? true :level (level @path) :child (:id row)}))

(defn delete-jus-event [_ row]
  (swap! table-state assoc-in [:delete-jus-modal] {:show? true :jusid (:id row)}))

(defn prikazi-naredbu [_ row]
  (.open js/window (str "pdf/" (:link-n row))))

(defn set-path [row]
  (reset! path  (-> (into (sorted-map-by #(< (js/parseInt (name %1)) (js/parseInt (name %2))))
                                                         (take-while #(not= (next-level (:level row)) (key %)) @path))
                                                   (assoc (next-level (:level row)) (:id row)))))

(defn icon-label [icon tip]
  (let [showing? (atom nil)
        ic-label [:div {:class         (str "zmdi " icon " rc-icon-smaller")
                        :style         {:color "#555657"}
                        :on-mouse-over #(reset! showing? 1)
                        :on-mouse-out  #(reset! showing? nil)}]]
    [box :class "display-inline-flex" :align :center :child [popover-tooltip
                                                             :label tip
                                                             :position :below-center
                                                             :showing? showing?
                                                             :anchor ic-label]]))

(defn count-veza [id]
  (if @count-veze
    (let [first-level-count @count-veze
          first-level-childs-count (:total (first-level-count id))
          first-level-ok-count (:locked (first-level-count id))]
      (loop [childs [id]
             count-all-childs 0
             count-ok-childs 0]
        (if (not-empty childs)
          (let [childs-data (for [child childs] [(:childs (first-level-count child)) (:total (first-level-count child)) (:locked (first-level-count child))])]
            (recur (set (remove nil? (flatten (map #(conj (first %)) childs-data))))
                   (reduce + count-all-childs (remove nil? (map second childs-data)))
                   (reduce + count-ok-childs (map last childs-data))))
          [count-all-childs count-ok-childs first-level-childs-count first-level-ok-count])))
    [0 0 0 0])
  )


(defn disable-del [& type]
  (fn [row]
    (if type
      ((constantly (first type)))
      (if (= (:veze row) 0) nil true))))

(defn disable-browse [& type]
  (fn [row]
    (if type
      ((constantly (first type)))
      (if (> (count (:link-n row)) 1) nil true))))

(def col-widths-types-naredbe {:naslov    {:type :label :width "58%" :field "JUSopis" :action :click :function set-path :double-click reset-path :tooltip "Puni naslov: "}
                               :glasnik   {:type :label :width "7%" :field "Glasnik" :action false}
                               :direktiva {:type :href :width "10%" :field "Direktiva" :href :link-d :action false}
                               :veze      {:type :label :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih naredbi: "}
                               :gotovo    {:type :label :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih naredbi: "}
                               :ok        {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :action lock-jus :tooltip "Napomena: "}
                               :fali      {:type :check-box :width "5%" :icon (icon-label "zmdi-close-circle-o" "Nema JUS standarda") :field "Fali" :disabled? nil :action fali-jus}
                               :akcije    {:type     :row-button :gap "2px" :width "5%" :justify :center :icon (icon-label "zmdi-wrench" "Uređivanje podataka")
                                           :children [{:id "r-b-1" :md-icon-name "zmdi zmdi-edit" :tooltip "Uredi nardbu" :tooltip-position :left-center :disabled? (disable-del nil) :action edit-nova-naredba-event}
                                                      {:id "r-b-2" :md-icon-name "zmdi zmdi-delete" :tooltip "Obriši naredbu" :tooltip-position :below-left :disabled? (disable-del) :action delete-jus-event}
                                                      {:id "r-b-3" :md-icon-name "zmdi zmdi-search-in-page" :tooltip "Prikaži naredbu" :tooltip-position :below-left :disabled? (disable-browse) :action prikazi-naredbu}]}})

(def col-widths-types-naredbe-stare {:naslov  {:type :label :width "60%" :field "JUSopis" :action :click :function set-path :double-click reset-path :tooltip "Puni naslov: "}
                                     :glasnik {:type :label :width "15%" :field "Glasnik" :action false :label "Sl. list SFRJ"}
                                     :veze    {:type :label :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih JUS standarda: "}
                                     :gotovo  {:type :label :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih JUS standarda: "}
                                     :ok      {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :action lock-jus :tooltip "Napomena: "}
                                     :fali    {:type :check-box :width "5%" :icon (icon-label "zmdi-close-circle-o" "Nema JUS standarda") :field "Fali" :disabled? nil :action fali-jus}
                                     :akcije  {:type     :row-button :gap "2px" :width "5%" :justify :center :icon (icon-label "zmdi-wrench" "Uređivanje podataka")
                                               :children [{:id "r-b-1" :md-icon-name "zmdi zmdi-edit" :tooltip "Uredi nardbu" :tooltip-position :left-center :disabled? (disable-del nil) :action edit-nova-naredba-event}
                                                          {:id "r-b-2" :md-icon-name "zmdi zmdi-delete" :tooltip "Obriši naredbu" :tooltip-position :below-left :disabled? (disable-del nil) :action delete-veza-event}
                                                          {:id "r-b-3" :md-icon-name "zmdi zmdi-search-in-page" :tooltip "Prikaži naredbu" :tooltip-position :below-left :disabled? (disable-browse) :action prikazi-naredbu}]}})


(defn col-widths-types-jus [alow-new-veza] {:jusid    {:type :label :width "15%" :field "JUSId" :action false :label "JUS"}
                                            :opis     {:type :label :width "58%" :field "JUSopis" :action :click :function set-path :label "Naslov" :double-click reset-path :tooltip "Puni naslov: "}
                                            :veze     {:type :label :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih JUS standarda: "}
                                            :gotovo   {:type :label :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih JUS standarda: "}
                                            :ok       {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :disabled? nil :action lock-jus}
                                            :fali     {:type :check-box :width "5%" :icon (icon-label "zmdi-close-circle-o" "Nema JUS standarda") :field "Fali" :disabled? nil :action fali-jus}
                                            :obavezan {:type :slider :width "5%" :min 0 :max 2 :step 1 :icon (icon-label "zmdi-alert-circle" "Sa obaveznom primjenom") :field "Mandatory" :disabled? nil :action obavezan-jus}
                                            :brisi    {:type     :row-button :gap "2px" :width "2%" :justify :end :icon (icon-label "zmdi-delete" "Brisanje reda")
                                                       :children [{:id               "r-b-1" :md-icon-name "zmdi zmdi-delete" :tooltip (if alow-new-veza "Obriši vezu!" "Ova veza je zaključana!")
                                                                   :tooltip-position :left-center :disabled? (disable-del nil) :action delete-veza-event}]}})

(defn find-selected [JUSId lev]
  (let [next-lev (next-level lev)
        path @path]
    (if (and (not-empty path) next-lev)
      (if (= JUSId (next-lev path))
        true nil) nil)))


(defn rows-naredbe [data screen]
  (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis Glasnik Direktiva Link-n Link-d Naredba Locked Napomena Fali]} x
                               count-veza (count-veza JUSId)]
                           {JUSId {:id          JUSId
                                   :naslov-full JUSopis
                                   :naslov      (h/cut-str-at JUSopis screen)
                                   :glasnik     Glasnik
                                   :direktiva   Direktiva
                                   :link-n      Link-n
                                   :link-d      Link-d
                                   :veze        (first count-veza)
                                   :gotovo      (second count-veza)
                                   :naredba     Naredba
                                   :ok          Locked
                                   :fali        Fali
                                   :napomena    Napomena
                                   :selected    (find-selected JUSId :0)
                                   ;:click     set-path
                                   :level       :0
                                   :tooltip     {:veze (nth count-veza 2) :gotovo (last count-veza) :naslov JUSopis :ok Napomena}}})) data)))

(defn rows-naredbe-stare [data screen level] (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis Glasnik Link-n Link-d Naredba Locked Napomena Fali]} x
                                                                          count-veza (count-veza JUSId)]
                                                                      (if (> Naredba 0)
                                                                        {JUSId {:id          JUSId
                                                                                :naslov-full JUSopis
                                                                                :naslov      (h/cut-str-at JUSopis screen)
                                                                                :glasnik     Glasnik
                                                                                :link-n      Link-n
                                                                                :link-d      Link-d
                                                                                :veze        (first count-veza)
                                                                                :gotovo      (second count-veza)
                                                                                :naredba     Naredba
                                                                                :ok          Locked
                                                                                :fali        Fali
                                                                                :napomena    Napomena
                                                                                :selected    (find-selected JUSId (:level data))
                                                                                ;:click     set-path
                                                                                :level       (:level data)
                                                                                :tooltip     {:veze (nth count-veza 2) :gotovo (last count-veza) :naslov JUSopis :ok Napomena}
                                                                                }} nil
                                                                        ))) (:data data))))

(defn rows-jus [data screen] (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis JUSgodina Locked Naredba Mandatory Fali]} x
                                                          count-veza (count-veza JUSId)]
                                                      (if (= Naredba 0)
                                                        {JUSId {:id       JUSId
                                                                :jusid    (str JUSId ":" JUSgodina)
                                                                :opis     (h/cut-str-at JUSopis screen)
                                                                :veze     (first count-veza)
                                                                :gotovo   (second count-veza)
                                                                :ok       Locked
                                                                :obavezan Mandatory
                                                                :fali     Fali
                                                                :selected (find-selected JUSId (:level data))
                                                                ;:click     set-path
                                                                :level    (:level data)
                                                                :tooltip  {:veze (nth count-veza 2) :gotovo (last count-veza) :opis JUSopis}
                                                                }} nil))) (:data data))))

(defn dropdown-data [data criteria]
  (into [] (filterv #(= criteria (:prefix %))
                    (sort-by :id (map (fn [x] (let [{:keys [JUSId]} x]
                                                {:id JUSId :label JUSId :prefix (first (clojure.string/split JUSId "."))})) data)))))

(defn dropdown-prefix [data]
  (sort-by :id (vec (into #{} (map (fn [x] (let [{:keys [JUSId]} x]
                                             {:id (first (clojure.string/split JUSId ".")) :label (first (clojure.string/split JUSId "."))})) (filterv #(= 0 (:Naredba %)) data))))))


(defn rows-level [level]
  (let [parent (level @path)
        criteria (->> (:veza @table-state)
                      (filterv #(= parent (:Parent %)))
                      (mapv :Child)
                      (into #{}))]
    {:data (filterv #(criteria (:JUSId %)) (:data @jus-data)) :level level}))


(defn resize []
  (fn [evt]
    (if-let [table (.item (.getElementsByClassName (h/get-elem "app") "rc-v-box display-flex rc-div-table") 1)]
      (swap! table-state assoc-in [:table-size] (/ (.-offsetWidth table) 12))
      )))

(defn splitter-props []
  nil
  )

(defn opis-level [level]
  (let [jus (first (filterv #(= (:JUSId %) (level @path)) (:data @jus-data)))]
    (if (> (:Naredba jus) 0)
      [0 (:JUSopis jus)]
      [1 (:JUSId jus)])))

(def show-level (atom nil))

(defn data-tables-level [level path screen]
  (let [alow-new-veza (alow-new-veza)
        col-widths-types-jus (col-widths-types-jus alow-new-veza)
        path-count (count path)
        level-int (js/parseInt (name level))]
    ^{:key level}
    [v-box
     :width "100%"
     :align :center
     :children
     [[title :level :level3 :style {:font-weight "bold" :font-family "Courier New" :align-self "center" :color ((prev-level level) colors)}
       :attr {:on-click #(reset-path {:level level})}
       :label (if (= (first (opis-level level)) 0) (str (second (opis-level level)) " veže:")
                                                   (str "JUS standard " (second (opis-level level)) " veže:"))]
      (if (< (- level-int 1) path-count (+ level-int 2))
        [scroller
         ;:size "flex-shrink"
         :margin "5px"
         :style {:margin-bottom "30px"}
         :v-scroll :auto
         :height "100%"
         :scroll :auto
         :align-self :center
         :width "95%"
         :max-width "1100px"
         :min-width "800px"
         :max-height "270px"
         :min-height "100px"
         :child [data-table (rows-jus (rows-level level) screen) col-widths-types-jus
                 level]]
        [line :size "1px" :color "lightgray" :style {:width "70%" :align-self "center" :background-color (:1 colors)}])
      ;[gap :size "30px"]
      ]]))




(defn entry-point []
  (r/create-class
    {
     ;:component-did-mount  #(do (events/listen js/window EventType.RESIZE (resize)))
     ;:component-did-update #(do ((resize)) (splitter-props))
     :reagent-render       (fn []
                             (let [path @path
                                   path-count (count path)
                                   jus-data (r/cursor jus-data [:data])
                                   jus-all-data @(r/cursor jus-all-data [:data])
                                   ;screen @(r/cursor table-state [:table-size])
                                   screen 80
                                   choice (:choice @showing)
                                   prefix (:prefix @showing)
                                   rows-naredbe (rows-naredbe (filter #(= (:Naredba %) 1) @jus-data) screen)
                                   alow-new-veza (alow-new-veza)
                                   v-height (* (.-innerHeight js/window) 0.8)]

                               (if (not-empty rows-naredbe)
                                 [:div
                                  [v-box
                                   :align :center
                                   :gap "2px"
                                   :width "100%"
                                   :height "100%"
                                   :children [[delete-dialog]
                                              [add-nova-naredba-dialog]
                                              [delete-jus-dialog]
                                              [edit-nova-naredba-dialog]
                                              [title :level :level2 :style {:font-weight "bold" :color "#555657"} :label "Veza usvojenih evropskih direktiva i JUS standarda "]
                                              [line :size "3px" :color "lightgray" :style {:width "90%" :align-self "center"}]
                                              [v-box
                                               :gap "2px"
                                               :align :center
                                               :width "100%"
                                               :height "auto"
                                               :children [[title :level :level3 :style {:font-weight "bold" :font-family "Courier New"}
                                                           :attr {:on-mouse-over #(reset! show-level :0)
                                                                  :on-mouse-out  #(reset! show-level nil)
                                                                  :on-click      #(reset-path {:level :1})
                                                                  }
                                                           :label "Naredbe vezane za evropske direktive"]
                                                          (if (< path-count 2)
                                                            [v-box
                                                             :gap "2px"
                                                             :align :center
                                                             :width "100%"
                                                             :height "auto"
                                                             :children
                                                             [[scroller
                                                               :style {:margin-left "auto" :margin-right "auto"}
                                                               :v-scroll :auto
                                                               :height "100%"
                                                               :scroll :auto
                                                               :width "85%"
                                                               :max-width "1100px"
                                                               :min-width "800px"
                                                               :max-height "270px"
                                                               ;:min-height "200px"
                                                               :child [data-table rows-naredbe col-widths-types-naredbe :0]]
                                                              [button
                                                               :label [:span "Nova naredba " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                               :on-click #(add-nova-naredba-event 1)
                                                               :disabled? (not= (count path) 0)
                                                               :style {:color            "white"
                                                                       :font-size        "14px"
                                                                       :background-color (:0 colors)
                                                                       :font-weight      "100"
                                                                       :border           "none"
                                                                       :padding          "7px 12px"}]]]
                                                            [line :size "1px" :color "lightgray" :style {:width "70%" :align-self "center" :background-color (:0 colors)}])
                                                          (if (> path-count 0) [title :level :level3 :style {:font-weight "bold" :align-self "center" :font-family "Courier New" :color (:0 colors)}
                                                                                :attr {:on-mouse-over #(reset! show-level :1)
                                                                                       :on-mouse-out  #(reset! show-level nil)
                                                                                       :on-click      #(reset-path {:level :2})}
                                                                                :label (str (second (opis-level :1)) " povlači:")])
                                                          (if (< 0 path-count 3)
                                                            [v-box
                                                             :gap "2px"
                                                             :align :center
                                                             :width "100%"
                                                             :height "auto"
                                                             :children
                                                             [[scroller
                                                               :v-scroll :auto
                                                               :height "100%"
                                                               :scroll :auto
                                                               :width "99%"
                                                               :max-width "1100px"
                                                               :min-width "800px"
                                                               :max-height "270px"
                                                               :child
                                                               (if (not-empty path)
                                                                 [data-table (rows-naredbe-stare (rows-level :1) screen 2) col-widths-types-naredbe-stare :1]
                                                                 [:div ""])]
                                                              [button
                                                               :label [:span "Stara naredba I " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                               :on-click #(add-nova-naredba-event 2)
                                                               :disabled? (not= (count path) 1)
                                                               :style {:color            "white"
                                                                       :font-size        "14px"
                                                                       :background-color (:1 colors)
                                                                       :border           "none"
                                                                       :padding          "7px 12px"}]]]
                                                            [line :size "1px" :color "lightgray" :style {:width "70%" :align-self "center" :background-color (:1 colors)}])
                                                          (if (> path-count 1) [title :level :level3 :style {:font-weight "bold" :align-self "center" :font-family "Courier New" :color (:1 colors)}
                                                                                :attr {:on-mouse-over #(reset! show-level :2)
                                                                                       :on-mouse-out  #(reset! show-level nil)
                                                                                       :on-click      #(reset-path {:level :3})}
                                                                                :label (str (second (opis-level :2)) " povlači:")])
                                                          (if (< 1 path-count 4)
                                                            [v-box
                                                             :gap "2px"
                                                             :align :center
                                                             :width "100%"
                                                             :height "auto"
                                                             :children
                                                             [
                                                              [scroller
                                                               :v-scroll :auto
                                                               :height "100%"
                                                               :scroll :auto
                                                               :width "99%"
                                                               :max-width "1100px"
                                                               :min-width "800px"
                                                               :max-height "270px"
                                                               :child
                                                               (if (not-empty path)
                                                                 [data-table (rows-naredbe-stare (rows-level :2) screen 3) col-widths-types-naredbe-stare :2]
                                                                 [:div ""])]
                                                              [button
                                                               :label [:span "Stara naredba II " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                               :on-click #(add-nova-naredba-event 3)
                                                               :disabled? (not= (count path) 2)
                                                               :style {:color            "white"
                                                                       :font-size        "14px"
                                                                       :background-color (:2 colors)
                                                                       :border           "none"
                                                                       :padding          "7px 12px"}]]]
                                                            [line :size "1px" :color "lightgray" :style {:width "70%" :align-self "center" :background-color (:2 colors)}])
                                                          (if (> path-count 1)
                                                            [scroller
                                                             :style {:margin-left "auto" :margin-right "auto" :border-style "solid" :border-color "lightgray" :border-width "0px"}
                                                             :v-scroll :auto
                                                             :height "100%"
                                                             :scroll :auto
                                                             :width "100%"
                                                             :max-width "1100px"
                                                             :min-width "400px"
                                                             :max-height (str (* v-height 0.8) "px")
                                                             :child [v-box
                                                                     :width "100%"
                                                                     :children [(doall (for [level-key (keys (rest path))] (data-tables-level level-key path screen)))
                                                                                (if (and (nil? choice) alow-new-veza (not-empty (rest path)))
                                                                                  [box :child "Dodaj JUS standard u listu veza."
                                                                                   :padding "6px"
                                                                                   :style {:margin-top       "5px"
                                                                                           :margin-bottom    "20px"
                                                                                           :color            "#333"
                                                                                           :background-color (:3 colors)
                                                                                           :border-top       "none"
                                                                                           :border-right     "none"
                                                                                           :border-bottom    "none"
                                                                                           :border-left      "4px solid #B5c113"
                                                                                           :border-radius    "0px"}]
                                                                                  [box
                                                                                   :align-self :center
                                                                                   :margin "5px"
                                                                                   :child (str choice " - " (:JUSopis (first (filterv #(= choice (:JUSId %)) jus-all-data))))])
                                                                                (if (not-empty path)
                                                                                  [h-box
                                                                                   :justify :center
                                                                                   :children [
                                                                                              [single-dropdown
                                                                                               :choices (dropdown-prefix jus-all-data)
                                                                                               :model prefix
                                                                                               :width "80px"
                                                                                               :max-height "100px"
                                                                                               :filter-box? true
                                                                                               :on-change
                                                                                               #(do (swap! showing assoc-in [:choice] nil) (swap! showing assoc-in [:prefix] %))]
                                                                                              [gap :size "4px"]
                                                                                              [single-dropdown
                                                                                               :choices (dropdown-data jus-all-data prefix)
                                                                                               :model choice
                                                                                               :width "150px"
                                                                                               :max-height "100px"
                                                                                               :filter-box? true
                                                                                               :on-change
                                                                                               #(swap! showing assoc-in [:choice] %)]
                                                                                              [gap :size "4px"]
                                                                                              [button
                                                                                               :label [:span "JUS " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                                                               :disabled? (or (not choice) (exist-veza) (not alow-new-veza) (< (count path) 2))
                                                                                               :on-click #(add-veza)
                                                                                               :style {:color            "white"
                                                                                                       :font-size        "14px"
                                                                                                       :background-color (:3 colors)
                                                                                                       :font-weight      "100"
                                                                                                       :border           "none"
                                                                                                       :padding          "7px 12px"}]]])
                                                                                [:div
                                                                                 (if (exist-veza)
                                                                                   [alert-box
                                                                                    :style {:margin-top       "20px"
                                                                                            :color            "#333"
                                                                                            :background-color "rgba(255, 0, 0, 0.1)"
                                                                                            :border-top       "none"
                                                                                            :border-right     "none"
                                                                                            :border-bottom    "none"
                                                                                            :border-left      "4px solid rgba(255, 0, 0, 0.8)"
                                                                                            :border-radius    "0px"}
                                                                                    ;:style {:margin-top "20px"}
                                                                                    :alert-type :danger
                                                                                    :body "Ovaj JUS standard je već unesen!"
                                                                                    :padding "6px"
                                                                                    :closeable? true
                                                                                    :on-close #(swap! showing assoc-in [:choice] nil)])
                                                                                 (if (and (not alow-new-veza) (not-empty path))
                                                                                   [alert-box
                                                                                    :style {:margin-top "20px"}
                                                                                    :alert-type :danger
                                                                                    :body "Zaključan je unos novih veza za ovaj standard!"
                                                                                    :padding "6px"
                                                                                    :closeable? false
                                                                                    :on-close #()])]
                                                                                [gap :size "220px"]]]])
                                                          [line
                                                           :size "3px"
                                                           :color "#555657"
                                                           :style {:width "90%" :align-self "center"}]
                                                          [gap :size "20px"]]]]]]
                                 [:div "Loading"])))}))


(defn ^:export main []
  (init-jus-data)
  (init-only-jus)
  (count-veze-fn)
  (f/set-options! {:button-primary {:attrs {:style {:margin "5px"}}}})
  (r/render-component (fn [] [entry-point]) (h/get-elem "app")))


(defn dummy-veza []
  (flatten (mapv (fn [x] (for [child (second x)]
                           (hash-map :Parent (first x) :Child child)))
                 (into [] (zipmap (mapv :JUSId (filter #(= (:Naredba %) 2) (:data @jus-data)))
                                  (mapv #(map :JUSId %) (partition 15 (:data @jus-all-data))))))))

(defn prepare-dummy-veza []
  (for [[child parent] (partition 2 (for [veza (select [ALL ALL LAST] (map vec (into #{} (dummy-veza))))] veza))]
    (GET "/jus/add-veza" {:params        {:parent parent :child child}
                          :handler       #(do (swap! showing assoc-in [:choice] nil) (init-veza [parent child]))
                          :error-handler #(do (js/alert (str "error: " %)) (init-veza))})))