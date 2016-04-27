(ns jus.jus
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [reagent.core :as r :refer [atom render-component]]
            [jus.helper :as h]
            [ajax.core :refer [GET POST json-response-format json-request-format url-request-format ajax-request]]
    ;[goog.events :as events]
            [reforms.reagent :include-macros true :as f]
            [re-com.core :as re-com :refer [h-box v-box box gap line row-button label checkbox horizontal-bar-tabs vertical-bar-tabs title p scroller single-dropdown button alert-box
                                            v-split h-split modal-panel]
             :refer-macros [handler-fn]]
            [jus.md-circle-icon-button :refer [icons example-icons]]
            [jus.utils :refer [panel-title title2 args-table material-design-hyperlink github-hyperlink status-text]]
            [re-com.popover :refer [popover-tooltip]]
            [jus.data-table :refer [data-table table-state nova-naredba-atom colors delete-dialog-template naredba-dialog-template
                                    cljs-ajax-upload-file jus-data path]])
  (:import [goog.events EventType]))



(def temp-path (atom nil))

(def jus-all-data (atom {:data nil}))

(def show-level (atom nil))

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
        data (if (= 1 (:fali row)) 0 1)]
        ;parent ((:level row) @path)

    (GET "/jus/update" {:params        {:filter criteria :field-data [{:Fali data}] :like false}
                        :handler       (fn [x] (if (= x 1) (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= id (:JUSId %)) :Fali] data y)))))

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
        (some #{child} (vals @path)))))


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

(defn add-nova-naredba []
  (let [data @nova-naredba-atom
        naslov {:JUSopis (:naslov data)}
        glasnik {:Glasnik (:glasnik data)}
        direktiva {:Direktiva (:direktiva data)}
        link {:Link-d (:link data)}
        godina {:JUSgodina (:godina data)}
        naredba {:Naredba (:naredba data)}
        X-CSRF-Token (:X-CSRF-Token data)
        file {:Link-n (case (:naredba data) 0 nil (cljs-ajax-upload-file "upload-file" X-CSRF-Token))}
        napomena {:Napomena (:napomena data)}
        ok {:Locked (:ok data)}
        old-id (naredba-exist (:naslov data))
        id (or old-id (:jusid data) (new-id))
        fields-data (merge naslov direktiva link godina naredba file ok napomena glasnik {:JUSId id :Locked 0})]
    (if (and (not= (:naredba data) 1) (not (exist-veza id))) (add-veza id))
    (if-not old-id
      (GET "/jus/insert" {:params        {:field-data [fields-data]}
                          :handler       #(init-jus-data)
                          :error-handler #(js/alert (str "error: " %))}))))


(defn add-nova-naredba-dialog []
  (let [process-ok (fn [event]
                     (add-nova-naredba)
                     (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :obavezan nil :X-CSRF-Token nil})
                     (swap! table-state assoc-in [:nova-naredba-modal] {:show? false}))
        process-cancel (fn [event]
                         (reset! nova-naredba-atom {:show? false :naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :obavezan nil :X-CSRF-Token nil})
                         (swap! table-state assoc-in [:nova-naredba-modal] {:show? false}))]
    (fn [] (when (:show? (:nova-naredba-modal @table-state)) [modal-panel :backdrop-color "grey"
                                                              :wrap-nicely? true
                                                              :backdrop-opacity 0.4
                                                              :style {:font-family "Consolas"}
                                                              :child [naredba-dialog-template
                                                                      process-ok
                                                                      process-cancel]]))))

(defn edit-nova-naredba []
  (let [data @nova-naredba-atom
        naredba {:Naredba (:naredba data)}
        jusid {:JUSId (:jusid data)}
        naslov {:JUSopis (:naslov data)}
        glasnik {:Glasnik (:glasnik data)}
        direktiva {:Direktiva (:direktiva data)}
        obavezan {:Mandatory (:obavezan data)}
        godina {:JUSgodina (:godina data)}
        link {:Link-d (:link data)}
        ok {:Locked (:ok data)}
        napomena {:Napomena (:napomena data)}
        X-CSRF-Token (:X-CSRF-Token data)
        file {:Link-n (case (:naredba data) 0 nil (or (cljs-ajax-upload-file "upload-file" X-CSRF-Token) (:file data)))}
        fields-data (merge naslov direktiva obavezan link file glasnik godina naredba ok napomena)]
    (GET "/jus/update" {:params        {:filter jusid :field-data [fields-data] :like false}
                        :handler       (fn [x] (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= (:jusid data) (:JUSId %))] (merge fields-data jusid) y))))
                        :error-handler #(js/alert (str "error: " %))})))

(defn edit-nova-naredba-dialog []
  (let [process-ok (fn [event]
                     (edit-nova-naredba)
                     (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :obavezan nil :napomena nil :X-CSRF-Token nil})
                     (swap! table-state assoc-in [:nova-naredba-modal] {:edit? false}))
        process-cancel (fn [event]
                         (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :obavezan nil :napomena nil :X-CSRF-Token nil})
                         (swap! table-state assoc-in [:nova-naredba-modal] {:edit? false}))]
    (fn [] (when (:edit? (:nova-naredba-modal @table-state)) [modal-panel :backdrop-color "grey"
                                                              :wrap-nicely? true
                                                              :backdrop-opacity 0.4
                                                              :style {:font-family "Consolas"}
                                                              :child [naredba-dialog-template
                                                                      process-ok
                                                                      process-cancel]]))))

(defn naredba-event [_ data]
  (reset! nova-naredba-atom {:jusid (:id data) :naslov (:naslov-full data) :glasnik (:glasnik data) :direktiva (:direktiva data)
                             :link  (:link-d data) :file (:link-n data) :godina (:godina data) :naredba (:naredba data)
                             :ok    (:ok data) :napomena (:napomena data) :obavezan (:obavezan data) :X-CSRF-Token (h/get-value "__anti-forgery-token")})
  (swap! table-state assoc-in [:nova-naredba-modal] (if (:id data) {:edit? true} {:show? true})))


(defn delete-veza-event [level row]
  (swap! table-state assoc-in [:delete-modal] {:show? true :level (level @path) :child (:id row)}))

(defn delete-jus-event [_ row]
  (swap! table-state assoc-in [:delete-jus-modal] {:show? true :jusid (:id row)}))

(defn prikazi-naredbu [_ row]
  (.open js/window (str "pdf/" (:link-n row))))

(defn set-path [row]
  (reset! path (-> (into (sorted-map-by #(< (js/parseInt (name %1)) (js/parseInt (name %2))))
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


(defn count-veza [id first-level-count]
  (if first-level-count
    (let [total-count (:total-count (first-level-count id))]
      (if total-count
        [(first total-count) 0 0 0]
       (loop [childs [id]
              all-childs #{}]
         (if (not-empty childs)
           (let [childs-data (vals (select-keys first-level-count childs))
                 result (into #{} (flatten (map :childs  childs-data)))]
               (recur  result  (into  all-childs  result)))
           (do (swap! count-veze assoc-in [id :total-count] [(count all-childs) 0])
               [(count all-childs) 0 0 0])))))
    [0 0 0 0]))





;;    (count-veza "1")

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

(def format-table
  (let [widts-nove {:naslov "57%" :glasnik "7%" :direktiva "10%" :veze "5%" :gotovo "5%" :ok "5%" :fali "5%" :akcije "5%"}
        widts-stare {:naslov "60%" :glasnik "15%" :veze "5%" :gotovo "5%" :ok "5%" :fali "5%" :akcije "5%"}
        widts-jus {:jusid "15%" :naslov "56%" :veze "5%" :gotovo "5%" :ok "5%" :fali "5%" :obavezan "5%" :brisi "4%"}
        jusid {:type :label :width "15%" :field "JUSId" :action false :label "JUS"}
        naslov {:type :label :width "60%" :field "JUSopis" :action :click :function set-path :double-click reset-path :tooltip "Puni naslov: "}
        opis {:type :label :label "Opis" :width "60%" :field "JUSopis" :action :click :function set-path :double-click reset-path :tooltip "Puni opis: "}
        glasnik {:type :label :width "7%" :field "Glasnik" :action false}
        direktiva {:type :href :width "10%" :field "Direktiva" :href :link-d :action false}
        veze {:type :label :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih naredbi: "}
        gotovo {:type :label :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih naredbi: "}
        ok {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :action lock-jus :tooltip "Napomena: "}
        fali {:type :check-box :width "5%" :icon (icon-label "zmdi-close-circle-o" "Nema JUS standarda") :field "Fali" :disabled? nil :action fali-jus}
        obavezan {:type :slider :width "5%" :min 0 :max 2 :step 1 :icon (icon-label "zmdi-alert-circle" "Sa obaveznom primjenom") :field "Mandatory" :disabled? nil :action obavezan-jus}
        akcije {:type     :row-button :gap "2px" :width "5%" :justify :center :icon (icon-label "zmdi-wrench" "Uređivanje podataka")
                :children [{:id "r-b-1" :md-icon-name "zmdi zmdi-edit" :tooltip "Uredi nardbu" :tooltip-position :left-center :disabled? (disable-del nil) :action naredba-event}
                           {:id "r-b-2" :md-icon-name "zmdi zmdi-delete" :tooltip "Obriši naredbu" :tooltip-position :below-left :disabled? (disable-del) :action delete-jus-event}
                           {:id "r-b-3" :md-icon-name "zmdi zmdi-search-in-page" :tooltip "Prikaži naredbu" :tooltip-position :below-left :disabled? (disable-browse) :action prikazi-naredbu}]}
        brisi {:type     :row-button :gap "2px" :width "4%" :justify :end :icon (icon-label "zmdi-delete" "Brisanje reda")
               :children [{:id "r-b-1" :md-icon-name "zmdi zmdi-edit" :tooltip "Uredi nardbu" :tooltip-position :left-center :disabled? (disable-del nil) :action naredba-event}
                          {:id               "r-b-2" :md-icon-name "zmdi zmdi-delete" :tooltip (if (alow-new-veza) "Obriši vezu!" "Ova veza je zaključana!")
                           :tooltip-position :left-center :disabled? (disable-del nil) :action delete-veza-event}]}]

    {:naredbe-nove  (merge-with #(merge %1 {:width %2})
                                {:naslov naslov :glasnik glasnik :direktiva direktiva :veze veze :gotovo gotovo :ok ok :fali fali :akcije akcije} widts-nove)
     :naredbe-stare (merge-with #(merge %1 {:width %2})
                                {:naslov naslov :glasnik glasnik :veze veze :gotovo gotovo :ok ok :fali fali :akcije akcije} widts-stare)
     :jus           (merge-with #(merge %1 {:width %2})
                                {:jusid jusid :naslov opis :veze veze :gotovo gotovo :ok ok :fali fali :obavezan obavezan :brisi brisi} widts-jus)}))


(defn find-selected [JUSId lev]
  (let [next-lev (next-level lev)
        path @path]
    (if (and (not-empty path) next-lev)
      (if (= JUSId (next-lev path))
        true nil) nil)))


(def table-fields
  {:nove  [:id :naslov-full :naslov :glasnik :direktiva :link-n :link-d :veze :gotovo :naredba :ok :fali :napomena :selected :level :tooltip]
   :stare [:id :naslov-full :naslov :glasnik :link-n :veze :gotovo :naredba :ok :fali :napomena :selected :level :tooltip]
   :jus   [:id :jusid :naslov-full :naslov :veze :gotovo :naredba :ok :fali :obavezan :napomena :selected :level :tooltip]})


(defn rows-naredbe [data screen type count-veze]
  (let [data (if (= (:level data) :2) (if (= type :jus) {:level (:level data) :data (filter #(= (:Naredba %) 0) (:data data))}
                                                        {:level (:level data) :data (filter #(> (:Naredba %) 0) (:data data))}) data)]
    (map #(vector (key %) (select-keys (val %) (type table-fields)))
         (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis JUSgodina Glasnik Direktiva Link-n Link-d Naredba Locked Mandatory Napomena Fali]} x
                                      count-veza (count-veza JUSId count-veze)]
                                  {JUSId {:id          JUSId
                                          :jusid       (str JUSId ":" JUSgodina)
                                          :naslov-full JUSopis
                                          :naslov      (h/cut-str-at JUSopis screen)
                                          :glasnik     Glasnik
                                          :godina      JUSgodina
                                          :direktiva   Direktiva
                                          :link-n      Link-n
                                          :link-d      Link-d
                                          :veze        (first count-veza)
                                          :gotovo      (second count-veza)
                                          :naredba     Naredba
                                          :ok          Locked
                                          :obavezan    Mandatory
                                          :fali        Fali
                                          :napomena    Napomena
                                          :selected    (find-selected JUSId (:level data))
                                          :level       (:level data)
                                          :tooltip     {:veze (nth count-veza 2) :gotovo (last count-veza) :naslov JUSopis :ok Napomena}}})) (:data data))))))


(defn dropdown-data [data criteria]
  (sort-by :id (filterv #(= criteria (:prefix %))
                        (mapv (fn [x] (let [{:keys [JUSId]} x]
                                        {:id JUSId :label JUSId :prefix (first (clojure.string/split JUSId "."))})) data))))

(def dropdown-prefix
  [{:id "A", :label "A"}
   {:id "B", :label "B"}
   {:id "C", :label "C"}
   {:id "D", :label "D"}
   {:id "E", :label "E"}
   {:id "F", :label "F"}
   {:id "G", :label "G"}
   {:id "H", :label "H"}
   {:id "I", :label "I"}
   {:id "J", :label "J"}
   {:id "K", :label "K"}
   {:id "L", :label "L"}
   {:id "M", :label "M"}
   {:id "N", :label "N"}
   {:id "P", :label "P"}
   {:id "R", :label "R"}
   {:id "U", :label "U"}
   {:id "Z", :label "Z"}])



(defn rows-level [level]
  (if (not= level :0)
    (let [parent (level @path)
          criteria (->> (:veza @table-state)
                        (filterv #(= parent (:Parent %)))
                        (mapv :Child)
                        (into #{}))]
      {:data (filterv #(criteria (:JUSId %)) (:data @jus-data)) :level level})
    {:data (filter #(= (:Naredba %) 1) (:data @jus-data)) :level level}))


(defn resize []
  (fn [evt]
    (if-let [table (.item (.getElementsByClassName (h/get-elem "app") "rc-v-box display-flex rc-div-table") 1)]
      (swap! table-state assoc-in [:table-size] (/ (.-offsetWidth table) 12)))))


(defn opis-level [level]
  (let [jus (first (filterv #(= (:JUSId %) (level @path)) (:data @jus-data)))]
    (if (> (:Naredba jus) 0)
      [0 (:JUSopis jus)]
      [1 (:JUSId jus)])))

(defn data-tables-level [level path screen count-veze]
  (let [path-count (count path)
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
         :child [data-table (rows-naredbe (rows-level level) screen :jus count-veze) (:jus format-table)
                 level]]
        [line :size "1px" :color "lightgray" :style {:width "70%" :align-self "center" :background-color (:1 colors)}])]]))

(defn entry-point []
  (r/create-class
    {;:component-did-mount  #(do (events/listen js/window EventType.RESIZE (resize)))
     ;:component-did-update #(do ((resize)) (splitter-props))
     :reagent-render (fn []
                       (let [at-path @path
                             path-count (count at-path)
                             jus-only (:data @jus-all-data)
                             ;screen @(r/cursor table-state [:table-size])
                             count-veze @count-veze
                             screen 80
                             choice (:choice @showing)
                             prefix (:prefix @showing)
                             alow-new-veza (alow-new-veza)
                             v-height (* (.-innerHeight js/window) 0.8)]

                         (if (not-empty [1])
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
                                        [title :level :level2 :style {:font-weight "bold" :color "#555657"} :label "Veza usvojenih evropskih direktiva i JUS standarda "
                                         :attr {:on-double-click #(reset! temp-path at-path)}]
                                        [line :size "3px" :color "lightgray" :style {:width "90%" :align-self "center"}]
                                        [v-box
                                         :gap "2px"
                                         :align :center
                                         :width "100%"
                                         :height "auto"
                                         :children [[title :level :level3 :style {:font-weight "bold" :font-family "Courier New"}
                                                     :attr {:on-mouse-over   #(reset! show-level :0)
                                                            :on-mouse-out    #(reset! show-level nil)
                                                            :on-click        #(reset-path {:level :1})
                                                            :on-double-click #(reset! path @temp-path)}

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
                                                         :child [data-table (rows-naredbe (rows-level :0) screen :nove count-veze) (:naredbe-nove format-table) :0]]
                                                        [button
                                                         :label [:span "Nova naredba " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                         :on-click #(naredba-event nil {:naredba 1})
                                                         :disabled? (not= (count at-path) 0)
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
                                                         (if (not-empty at-path)
                                                           [data-table (rows-naredbe (rows-level :1) screen :stare count-veze) (:naredbe-stare format-table) :1]
                                                           [:div ""])]
                                                        [button
                                                         :label [:span "Stara naredba I " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                         :on-click #(naredba-event nil {:naredba 2})
                                                         :disabled? (not= (count at-path) 1)
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
                                                       [[scroller
                                                         :v-scroll :auto
                                                         :height "100%"
                                                         :scroll :auto
                                                         :width "99%"
                                                         :max-width "1100px"
                                                         :min-width "800px"
                                                         :max-height "270px"
                                                         :child
                                                         (if (not-empty at-path)
                                                           [data-table (rows-naredbe (rows-level :2) screen :stare count-veze) (:naredbe-stare format-table) :2]
                                                           [:div ""])]
                                                        [button
                                                         :label [:span "Stara naredba II " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                         :on-click #(naredba-event nil {:naredba 3})
                                                         :disabled? (not= (count at-path) 2)
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
                                                               :children [(doall (for [level-key (keys (rest at-path))] (data-tables-level level-key at-path screen count-veze)))
                                                                          (if (and (nil? choice) alow-new-veza (not-empty (rest at-path)))
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
                                                                             :child (str choice " - " (:JUSopis (first (filterv #(= choice (:JUSId %)) jus-only))))])
                                                                          (if (not-empty at-path)
                                                                            [h-box
                                                                             :justify :center
                                                                             :children [
                                                                                        [single-dropdown
                                                                                         :choices dropdown-prefix
                                                                                         :model prefix
                                                                                         :width "80px"
                                                                                         :max-height "100px"
                                                                                         :filter-box? true
                                                                                         :on-change
                                                                                         #(do (swap! showing assoc-in [:choice] nil) (swap! showing assoc-in [:prefix] %))]
                                                                                        [gap :size "4px"]
                                                                                        [single-dropdown
                                                                                         :choices (dropdown-data jus-only prefix)
                                                                                         :model choice
                                                                                         :width "150px"
                                                                                         :max-height "100px"
                                                                                         :filter-box? true
                                                                                         :on-change
                                                                                         #(swap! showing assoc-in [:choice] %)]
                                                                                        [gap :size "4px"]
                                                                                        [button
                                                                                         :label [:span "JUS " [:i.zmdi.zmdi-hc-fw-rc.zmdi-plus]]
                                                                                         :disabled? (or (exist-veza) (not alow-new-veza) (< (count at-path) 2))
                                                                                         :on-click #(if choice (add-veza) (naredba-event nil {:naredba 0}))
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
                                                                              :alert-type :danger
                                                                              :body "Ovaj JUS standard je već unesen!"
                                                                              :padding "6px"
                                                                              :closeable? true
                                                                              :on-close #(swap! showing assoc-in [:choice] nil)])
                                                                           (if (and (not alow-new-veza) (not-empty at-path))
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
  (count-veze-fn)
  (init-only-jus)
  (f/set-options! {:button-primary {:attrs {:style {:margin "5px"}}}})
  (r/render-component (fn [] [entry-point]) (h/get-elem "app")))
