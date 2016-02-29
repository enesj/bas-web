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
						[jus.data-table :refer [data-table]])
	(:import [goog.events EventType]))


(def jus-data (atom {:data nil}))

(def jus-all-data (atom {:data nil}))

(def table-state (atom {:table-size       60 :veza nil :path nil
												:delete-modal     {:show? false :level nil :child nil} :nova-naredba-modal {:show? false :edit? false :naslov nil :direktiva nil :link nil :file nil}
												:delete-jus-modal {:show? false :jusid nil}}))

(def count-veze (atom nil))

(defn count-veze-fn []
	(GET "/jus/count-veze" {:handler       #(reset! count-veze %)
													:error-handler #(println "some error occured: " %)}))



(def showing (atom {:hover? false :choice nil :prefix "A"}))

(def nova-naredba-atom (atom {:jusid nil :naslov nil :direktiva nil :link nil :file nil :glasnik nil :naredba nil :X-CSRF-Token nil}))


(defn init-jus-data []
	;(println (count data))
	(GET "/jus/active-data" {:handler       #(do (swap! jus-data assoc-in [:data] (first %)) (swap! table-state assoc-in [:veza] (second %)))
													 :error-handler #(println "some error occured: " %)})
	)

(defn init-only-jus []
	(GET "/jus/only-jus" {

												:handler       #(swap! jus-all-data assoc-in [:data] %)
												:error-handler #(println "some error occured: " %)}))






(defn alow-new-veza []
	(let [path (r/cursor table-state [:path])]
		;(println "alow-new-veza" who)
		(if (not-empty @path)
			(let [lock (:Locked (first (filterv #(= (:JUSId %) (val (last @path))) (:data @jus-data))))]
				(if (= lock 0) true false)))))


(defn delete-dialog-template [label process-ok process-cancel]
	[h-box
	 :gap "12px"
	 :children [[title :level :level3 :style {:font-weight "bold" :align-self "center"} :label label]
							[button
							 :label "Briši"
							 :class "btn-primary"
							 :on-click process-ok]
							[button
							 :label "Cancel"
							 :on-click process-cancel]]])


(defn all-childs [id]
	(let [veza (:veza @table-state)
				first-level-childs (mapv :Child (filterv #(= id (:Parent %)) veza))]
		(loop [childs first-level-childs
					 all-childs []]
			(if (not-empty childs)
				(recur (mapv :Child (flatten (doall (for [id childs] (filterv #(= id (:Parent %)) veza))))) (into all-childs childs))
				all-childs
				))))

(defn is-child? [id child]
	(some #{child} (all-childs id)))


(defn init-veza [& opt]
	(if opt
		(let [[parent child] (first opt)]
			(swap! table-state update-in [:veza] #(merge % {:Parent parent :Child child})))
		(GET "/jus/veza" {:handler       #(swap! table-state assoc-in [:veza] %)
											:error-handler #(println "some error occured: " %)})))



(defn current-parent-child [& id]
	(let [local-state @table-state
				choice (:choice @showing)
				path (:path local-state)
				child (if id (first id) choice)
				parent (or ((clojure.set/map-invert (:path local-state)) child) ((keyword (str (count path))) path))
				;parent (parent path)
				]
		;(if id (println id child (child (:path local-state)) (keyword (str (count path)))))
				[parent child]))


(defn lock-jus [row]
	(let [id (:id row)
				criteria {:JUSId id}
				data (if (= 1 (:ok row)) 0 1)
				parent ((:level row) (:path @table-state))]
		(GET "/jus/update" {:params        {:filter criteria :field-data [{:Locked data}] :like false}
												:handler       (fn [x] (do (if (= x 1) (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= id (:JUSId %)) :Locked] data y))))
																									 (swap! count-veze update-in [parent :locked]
																													(fn [x] (if (= 0 data) (dec x) (inc x))))))
												:error-handler #(println "some error occured: " %)})))


(defn obavezan-jus [row data]
	(let [id (:id row)
				criteria {:JUSId id}]
		;(println "obv")
		(GET "/jus/update" {:params        {:filter criteria :field-data [{:Mandatory data}] :like false}
												:handler       (fn [x] (if (= x 1) (swap! jus-data update-in [:data] (fn [y] (setval [ALL #(= id (:JUSId %)) :Mandatory] data y)))))
												:error-handler #(println "some error occured: " %)})))

(defn exist-veza []
	(or (not-empty (filterv #(= % {:Parent (first (current-parent-child)) :Child (second (current-parent-child))}) (:veza @table-state)))
			(= (first (current-parent-child)) (second (current-parent-child)))
			(is-child? (second (current-parent-child)) (first (current-parent-child)))
			))


(defn add-veza [& id]
	(let [jus-id (if id (first id) (:choice @showing))
				[parent child] (current-parent-child jus-id)
				new (first (filterv #(= jus-id (:JUSId %)) (:data @jus-all-data)))]
		(GET "/jus/add-veza" {:params        {:parent parent :child child}
													:handler       #(swap! count-veze update-in [parent]
																								 (fn [x] {:total (inc (:total x)) :locked (+ (:locked x) (:Locked new)) :childs (conj (:childs x) child)}))
													:error-handler #(println "some error occured: " %)})
		(if-not id
			(do
				(swap! showing assoc-in [:choice] nil)
				(init-veza [parent child])
				(if-not (some #(= (:JUSId %) (:JUSId new)) (:data @jus-data))
					(swap! jus-data update-in [:data] #(merge % new)))))
		))

(defn next-level [level]
	(keyword (str (inc (js/parseInt (name level))))))

(defn reset-path [row]
	(swap! table-state update-in [:path] (fn [x] (into {} (take-while #(not= (next-level (:level row)) (key %)) x)))))

(defn delete-veza []
	(let [local-state @table-state
				{:keys [level child]} (:delete-modal local-state)
				old (first (filterv #(= child (:JUSId %)) (:data @jus-data)))]
		(do (GET "/jus/del-veza" {:params        {:parent level :child child}
															:handler       #(init-veza)
															:error-handler #(println "some error occured: " %)})
				(swap! count-veze update-in [level]
							 (fn [x] {:total (dec (:total x)) :locked (- (:locked x) (:Locked old)) :childs (remove #{child} (:childs x))}))
				(reset-path {:level ((clojure.set/map-invert (:path local-state)) level)})
				)))


(defn delete-dialog []
	(let [process-ok (fn [event]
										 (delete-veza)
										 (swap! table-state assoc-in [:delete-modal] {:show? false :level nil :child nil}))
				process-cancel (fn [event] (swap! table-state assoc-in [:delete-modal] {:show? false :level nil :child nil}))]
		(fn []
			(when (:show? (:delete-modal @table-state)) [modal-panel
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
											:error-handler #(println "some error occured: " %)}))

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
		(fn []
			(when (:show? (:delete-jus-modal @table-state)) [modal-panel :backdrop-color "grey"
																											 :wrap-nicely? true
																											 :backdrop-opacity 0.4
																											 :style {:font-family "Consolas"}
																											 :child [delete-jus-dialog-template
																															 process-ok
																															 process-cancel]]))))



(defn cljs-ajax-upload-file [element-id anti-forgery-token]
	(let [el (.getElementById js/document element-id)
				name (.-name el)
				file (aget (.-files el) 0)
				form-data (doto
										(js/FormData.)
										(.append name file))]
		(if file
			(do (ajax-request
						{:uri             "/jus/upload"
						 :method          "POST"
						 :body            form-data
						 :handler         #()
						 :headers         {:X-CSRF-Token anti-forgery-token}
						 :response-format (json-response-format {:keywords? true})})
					(.-name file)))))


(defn upload-component []
	[:input {:style {:margin-bottom "10px"}
					 :type  "file"
					 :name  "upload-file"
					 :id    "upload-file"}])

(defn new-id []
	(inc (js/parseInt (:JUSId (apply max-key #(js/parseInt (:JUSId %)) (filter #(> (:Naredba %) 0) (:data @jus-data)))))))

(defn add-nova-naredba-dialog-template [process-ok process-cancel]
	[v-box
	 :width "700px"
	 :children [(if (= (:naredba @nova-naredba-atom) 1)
								(f/panel
									"Dodaj naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov])
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "Direktiva" nova-naredba-atom [:direktiva])
										(f/text "Link" nova-naredba-atom [:link])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel)))))
								(f/panel
									"Dodaj naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov])
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel)))
										)))

							]])


(defn add-nova-naredba []
	(let [data @nova-naredba-atom
				naslov {:JUSopis (:naslov data)}
				glasnik {:Glasnik (:glasnik data)}
				direktiva {:Direktiva (:direktiva data)}
				link {:Link-d (:link data)}
				X-CSRF-Token (:X-CSRF-Token data)
				file {:Link-n (cljs-ajax-upload-file "upload-file" X-CSRF-Token)}
				naredba {:Naredba (:naredba data)}
				id (new-id)
				fields-data (merge naslov direktiva link file naredba glasnik {:JUSId id :Locked 0})]
		(if (= (:naredba data) 2) (add-veza id))
		(GET "/jus/insert" {:params        {:field-data [fields-data]}
												:handler       #(init-jus-data)
												:error-handler #(println "some error occured: " %)})))



(defn add-nova-naredba-dialog []
	(let [process-ok (fn [event]
										 (add-nova-naredba)
										 (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :X-CSRF-Token nil})
										 (swap! table-state assoc-in [:nova-naredba-modal] {:show? false}))
				process-cancel (fn [event]
												 (reset! nova-naredba-atom {:show? false :naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :X-CSRF-Token nil})
												 (swap! table-state assoc-in [:nova-naredba-modal] {:show? false}))]
		(fn []
			(when (:show? (:nova-naredba-modal @table-state)) [modal-panel :backdrop-color "grey"
																												 :wrap-nicely? true
																												 :backdrop-opacity 0.4
																												 :style {:font-family "Consolas"}
																												 :child [add-nova-naredba-dialog-template
																																 process-ok
																																 process-cancel]]))))



(defn edit-nova-naredba []
	;(println "dsaf " (new-id))
	(let [data @nova-naredba-atom
				jusid {:JUSId (:jusid data)}
				naslov {:JUSopis (:naslov data)}
				glasnik {:Glasnik (:glasnik data)}
				direktiva {:Direktiva (:direktiva data)}
				link {:Link-d (:link data)}
				X-CSRF-Token (:X-CSRF-Token data)
				file {:Link-n (or (cljs-ajax-upload-file "upload-file" X-CSRF-Token) (:file data))}
				fields-data (merge naslov direktiva link file glasnik)
				]
		(GET "/jus/update" {:params        {:filter jusid :field-data [fields-data] :like false}
												:handler       #(init-jus-data)
												:error-handler #(println "some error occured: " %)})))

(defn edit-nova-naredba-dialog-template [process-ok process-cancel]
	[v-box
	 :width "700px"
	 :children [
							(if (= (:naredba @nova-naredba-atom) 1)
								(f/panel
									"Uredi naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov])
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "Direktiva" nova-naredba-atom [:direktiva])
										(f/text "Link" nova-naredba-atom [:link])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel)))
										))
								(f/panel
									"Dodaj naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov])
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel)))
										))
								)]])

(defn edit-nova-naredba-dialog []
	(let [process-ok (fn [event]
										 (edit-nova-naredba)
										 (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :X-CSRF-Token nil})
										 (swap! table-state assoc-in [:nova-naredba-modal] {:edit? false}))
				process-cancel (fn [event]
												 (reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba nil :X-CSRF-Token nil})
												 (swap! table-state assoc-in [:nova-naredba-modal] {:edit? false}))]
		(fn []
			(when (:edit? (:nova-naredba-modal @table-state)) [modal-panel :backdrop-color "grey"
																												 :wrap-nicely? true
																												 :backdrop-opacity 0.4
																												 :style {:font-family "Consolas"}
																												 :child [edit-nova-naredba-dialog-template
																																 process-ok
																																 process-cancel]]))))


(defn add-nova-naredba-event [naredba]
	(reset! nova-naredba-atom {:naslov nil :glasnik nil :direktiva nil :link nil :file nil :naredba naredba :X-CSRF-Token (h/get-value "__anti-forgery-token")})
	(swap! table-state assoc-in [:nova-naredba-modal] {:show? true})
	)

(defn edit-nova-naredba-event [_ _ _ data]
	(reset! nova-naredba-atom {:jusid (:id data) :naslov (:naslov-full data) :glasnik (:glasnik data) :direktiva (:direktiva data)
														 :link  (:link-d data) :file (:link-n data) :naredba (:naredba data) :X-CSRF-Token (h/get-value "__anti-forgery-token")})
	(swap! table-state assoc-in [:nova-naredba-modal] {:edit? true})
	)


(defn delete-veza-event [level child _]
	(swap! table-state assoc-in [:delete-modal] {:show? true :level level :child child}))

(defn delete-jus-event [_ jusid _]
	(swap! table-state assoc-in [:delete-jus-modal] {:show? true :jusid jusid}))

(defn prikazi-naredbu [_ _ link]
	(.open js/window (str "pdf/" link)))



(defn set-path [JUSId level]
	(swap! table-state update-in [:path] (fn [x] (-> (into {} (take-while #(not= (next-level level) (key %)) x))
																									 (assoc (next-level level) JUSId)
																									 ))))



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

						(recur (remove nil? (flatten (map #(conj (first %)) childs-data)))
									 (reduce + count-all-childs (remove nil? (map second childs-data)))
									 (reduce + count-ok-childs (map last childs-data))))
					[count-all-childs count-ok-childs first-level-childs-count first-level-ok-count]
					)))
		[0 0 0 0]))


(defn disable-del [& type]
	(fn [row]
		;(println "type" type (first type))
		(if type
			((constantly (first type)))
			(if (= (:veze row) 0) nil true))))

(defn disable-browse [& type]
	(fn [row]
		;(println "type" type (first type))
		(if type
			((constantly (first type)))
			(if (> (count (:link-n row)) 1) nil true))))


((disable-del nil) {:veze 1})


(def col-widths-types-naredbe {:naslov    {:type :label :width "63%" :field "JUSopis" :action :click :function set-path :double-click reset-path}
															 :glasnik   {:type :label :width "7%" :field "Glasnik" :action false}
															 :direktiva {:type :href :width "10%" :field "Direktiva" :href :link-d :action false}
															 :veze      {:type :label-tooltip :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih naredbi: "}
															 :gotovo    {:type :label-tooltip :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih naredbi: "}
															 :ok        {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :action lock-jus}
															 :akcije    {:type     :row-button :gap "2px" :width "5%" :justify :center :icon (icon-label "zmdi-wrench" "Uređivanje podataka")
																					 :children [{:id "r-b-1" :md-icon-name "zmdi zmdi-edit" :tooltip "Uredi nardbu" :tooltip-position :left-center :disabled? (disable-del nil) :action edit-nova-naredba-event}
																											{:id "r-b-2" :md-icon-name "zmdi zmdi-delete" :tooltip "Obriši naredbu" :tooltip-position :below-left :disabled? (disable-del) :action delete-jus-event}
																											{:id "r-b-3" :md-icon-name "zmdi zmdi-search-in-page" :tooltip "Prikaži naredbu" :tooltip-position :below-left :disabled? (disable-browse) :action prikazi-naredbu}
																											]}})

(def col-widths-types-naredbe-stare {:naslov  {:type :label :width "65%" :field "JUSopis" :action :click :function set-path :double-click reset-path}
																		 :glasnik {:type :label :width "15%" :field "Glasnik" :action false :label "Sl. list SFRJ"}
																		 :veze    {:type :label-tooltip :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih JUS standarda: "}
																		 :gotovo  {:type :label-tooltip :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih JUS standarda: "}
																		 :ok      {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :action lock-jus}
																		 :akcije  {:type     :row-button :gap "2px" :width "5%" :justify :center :icon (icon-label "zmdi-wrench" "Uređivanje podataka")
																							 :children [{:id "r-b-1" :md-icon-name "zmdi zmdi-edit" :tooltip "Uredi nardbu" :tooltip-position :left-center :disabled? (disable-del nil) :action edit-nova-naredba-event}
																													{:id "r-b-2" :md-icon-name "zmdi zmdi-delete" :tooltip "Obriši naredbu" :tooltip-position :below-left :disabled? (disable-del nil) :action delete-veza-event}
																													{:id "r-b-3" :md-icon-name "zmdi zmdi-search-in-page" :tooltip "Prikaži naredbu" :tooltip-position :below-left :disabled? (disable-browse) :action prikazi-naredbu}
																													]}})

;Mandatory

(defn col-widths-types-jus [alow-new-veza] {:jusid    {:type :label :width "15%" :field "JUSId" :action false :label "JUS"}
																						:opis     {:type :label :width "63%" :field "JUSopis" :action :click :function set-path :label "Naslov" :double-click reset-path}
																						:veze     {:type :label-tooltip :width "5%" :icon (icon-label "zmdi-attachment-alt" "Ukupan broj vezanih standarda") :field nil :action false :tooltip "Broj direktno vezanih JUS standarda: "}
																						:gotovo   {:type :label-tooltip :width "5%" :icon (icon-label "zmdi-key" "Ukupan broj završenih vezanih standarda") :field nil :action false :tooltip "Završeno direktno vezanih JUS standarda: "}
																						:ok       {:type :check-box :width "5%" :icon (icon-label "zmdi-shield-check" "Završen unos vezanih standarda") :field "Locked" :disabled? nil :action lock-jus}
																						:obavezan {:type :slider :width "5%" :min 0 :max 2 :step 1 :icon (icon-label "zmdi-alert-circle" "Sa obaveznom primjenom") :field "Mandatory" :disabled? nil :action obavezan-jus
																											 :style-value  {:color "blue"}}
																						:brisi    {:type     :row-button :gap "2px" :width "2%" :justify :end :icon (icon-label "zmdi-delete" "Brisanje reda")
																											 :children [{:id               "r-b-1" :md-icon-name "zmdi zmdi-delete" :tooltip (if alow-new-veza "Obriši vezu!" "Ova veza je zaključana!")
																																	 :tooltip-position :left-center :disabled? (disable-del nil) :action delete-veza-event}]}})

(defn find-selected [JUSId lev]
	(let [next-lev (next-level lev)
				path (:path @table-state)]
		(if (and (not-empty path) next-lev)
			(if (= JUSId (next-lev path))
				true
				nil) nil)))



(defn rows-naredbe [data screen] (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis Glasnik Direktiva Link-n Link-d Naredba Locked]} x
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
																																	:selected    (find-selected JUSId :0)
																																	;:click     set-path
																																	:level       :0
																																	:tooltip     {:veze (nth count-veza 2) :gotovo (last count-veza)}
																																	}}
																													)) data)))


(defn rows-naredbe-stare [data screen] (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis Glasnik Link-n Link-d Naredba Locked]} x
																																		count-veza (count-veza JUSId)]
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
																																				:selected    (find-selected JUSId (:level data))
																																				;:click     set-path
																																				:level       (:level data)
																																				:tooltip     {:veze (nth count-veza 2) :gotovo (last count-veza)}
																																				}}
																																)) (:data data))))

(defn rows-jus [data screen] (into {} (mapv (fn [x] (let [{:keys [JUSId JUSopis JUSgodina Locked Mandatory]} x
																													count-veza (count-veza JUSId)]
																											{JUSId {:id       JUSId
																															:jusid    (str JUSId ":" JUSgodina)
																															:opis     (h/cut-str-at JUSopis screen)
																															:veze     (first count-veza)
																															:gotovo   (second count-veza)
																															:ok       Locked
																															:obavezan Mandatory
																															:selected (find-selected JUSId (:level data))
																															;:click     set-path
																															:level    (:level data)
																															:tooltip  {:veze (nth count-veza 2) :gotovo (last count-veza)}
																															}}
																											)) (:data data))))



(defn dropdown-data [data criteria]
	(into [] (filterv #(= criteria (:prefix %))
										(sort-by :id (map (fn [x] (let [{:keys [JUSId]} x]
																								{:id JUSId :label JUSId :prefix (first (clojure.string/split JUSId "."))}
																								)) data)))))

(defn dropdown-prefix [data]
	(sort-by :id (vec (into #{} (map (fn [x] (let [{:keys [JUSId]} x]
																						 {:id (first (clojure.string/split JUSId ".")) :label (first (clojure.string/split JUSId "."))}))
																	 (filterv #(= 0 (:Naredba %)) data))))))


(defn rows-level [level]
	(let [parent (level (:path @table-state))
				criteria (->> (:veza @table-state)
											(filterv #(= parent (:Parent %)))
											(mapv :Child)
											(into #{}))]
		{:data (filterv #(criteria (:JUSId %)) (:data @jus-data)) :level level}))


(defn resize []
	(fn [evt]
		(if-let [table (.item (.getElementsByClassName (h/get-elem "app") "rc-v-box display-flex rc-div-table") 1)]
			(swap! table-state assoc-in [:table-size] (/ (.-offsetWidth table) 10.5))
			)))

(defn splitter-props []
	(let [splitter0 (.item (.getElementsByClassName (h/get-elem "app") "display-flex re-v-split-splitter") 0)
				splitter1 (.item (.getElementsByClassName (h/get-elem "app") "display-flex re-v-split-splitter") 1)]
		(if splitter0 (aset splitter0 "style" "flex: 0 0 20px	; cursor: row-resize; border-top: 1px solid ; border-bottom: 1px solid; border-color: lightgray;"))
		(if splitter1 (aset splitter1 "style" "flex: 0 0 20px	; cursor: row-resize; border-top: 1px solid;  border-bottom: 1px solid; border-color: lightgray;"))))



(defn opis-level [level]
	(let [jus (first (filterv #(= (:JUSId %) (level (:path @table-state))) (:data @jus-data)))]
		(if (> (:Naredba jus) 0)
			[0 (:JUSopis jus)]
			[1 (:JUSId jus)])))


(defn data-tables-level [level path screen]
	(let [alow-new-veza (alow-new-veza)
				col-widths-types-jus (col-widths-types-jus alow-new-veza)]
		^{:key level}
		;(println "l-l " level)
		[v-box
		 ;:gap      "10px"
		 :width "100%"
		 :align :center
		 :children
		 [[title :level :level3 :style {:font-weight "bold" :font-family "Courier New" :align-self "center"}
			 :label (if (= (first (opis-level level)) 0) (str (second (opis-level level)) " veže:")
																									 (str "JUS standard " (second (opis-level level)) " veže:"))]
			[scroller
			 ;:size "flex-shrink"
			 :margin "10px"
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
							 (level path)]]
			[gap :size "30px"]]]))



(defn entry-point []

	(r/create-class
		{
		 :component-did-mount  #(do (events/listen js/window EventType.RESIZE (resize)))
		 :component-did-update #(do ((resize))
																(splitter-props)
																)
		 :reagent-render       (fn []
														 (let [
																	 state-local @table-state
																	 path @(r/cursor table-state [:path])
																	 jus-data @(r/cursor jus-data [:data])
																	 jus-all-data @(r/cursor jus-all-data [:data])
																	 screen @(r/cursor table-state [:table-size])
																	 choice (:choice @showing)
																	 prefix (:prefix @showing)
																	 rows-naredbe (rows-naredbe (filterv #(= (:Naredba %) 1) jus-data) screen)
																	 alow-new-veza (alow-new-veza)
																	 ;rows-naredbe-stare (rows-naredbe-stare (filter #(= (:Naredba %) 2) jus-data) screen)
																	 ]
															 (if (not-empty rows-naredbe)
																 [:div
																	[v-box
																	 :align :center
																	 :gap "10px"
																	 :width "100%"
																	 :height "80%"
																	 :children [
																							[delete-dialog]
																							[add-nova-naredba-dialog]
																							[delete-jus-dialog]
																							[edit-nova-naredba-dialog]
																							[title :level :level2 :style {:font-weight "bold" :color "#555657"} :label "Veza usvojenih evropskih direktiva i JUS standarda "]
																							[line
																							 :size "3px"
																							 :color "lightgray"
																							 ;;                                                             :color  "#4d90fe"
																							 :style {:width "90%" :align-self "center"}]
																							[v-box
																							 :gap "5px"
																							 :align :center
																							 :width "100%"
																							 :height "auto"
																							 :children [[title :level :level3 :style {:font-weight "bold" :font-family "Courier New"} :label "Naredbe vezane za evropske direktive"]
																													[v-split
																													 :style {:margin-left "auto" :margin-right "auto"}
																													 :size "700px"
																													 :width "70%"
																													 :initial-split "70%"
																													 ;:size "0 1 auto"
																													 ;:splitter-size "25px"
																													 ;:on-split-change #(swap! table-state assoc-in [:m-heght-panel2] (str (+ 600 (- 300 (* % 8))) "px"))
																													 :panel-1
																													 [v-box
																														:gap "5px"
																														:align :center
																														:width "100%"
																														:height "auto"
																														:children
																														[
																														 [v-split
																															:size "600px"
																															:width "100%"
																															:initial-split "65%"
																															;:size "1 0 auto"
																															:panel-1-max-h 350

																															;:splitter-size "25px"
																															;:on-split-change #(swap! table-state assoc-in [:m-heght-panel2] (str (+ 600 (- 300 (* % 8))) "px"))
																															:panel-1
																															[v-box
																															 :gap "5px"
																															 :align :center
																															 :width "100%"
																															 :min-height "350px"
																															 ;:max-height "500px"
																															 :children [
																																					[scroller
																																					 :style {:margin-left "auto" :margin-right "auto"}
																																					 :v-scroll :auto
																																					 :height "100%"
																																					 :scroll :auto
																																					 :width "85%"
																																					 :max-width "1100px"
																																					 :min-width "800px"
																																					 :max-height "270px"
																																					 :min-height "200px"
																																					 :child [data-table rows-naredbe col-widths-types-naredbe :0]]
																																					[button
																																					 :label [:span "Nova naredba " [:i.zmdi.zmdi-hc-fw-rc.zmdi-collection-plus]]
																																					 :disabled? (not= (count path) 0)
																																					 :on-click #(add-nova-naredba-event 1)
																																					 :style {:color            "white"
																																									 :font-size        "14px"
																																									 ;:background-color (if (:hover? @table-state) "#0072bb" "#4d90fe")
																																									 ;:background-color (if hover? "#0072bb" "#4d90fe")
																																									 :background-color (if (:hover? @showing) "#B5c113" "#B5D113	")
																																									 :font-weight      "100"
																																									 :border           "none"
																																									 :padding          "7px 12px"
																																									 ;:margin-top       "10px"
																																									 }
																																					 ;:attr {:on-mouse-over (handler-fn (swap! showing assoc-in [:hover?] true))
																																					 ;			:on-mouse-out  (handler-fn (swap! showing assoc-in [:hover?] false))}
																																					 ]
																																					]]
																															:panel-2
																															[v-box
																															 :gap "5px"
																															 :align :center
																															 :width "100%"
																															 :height "320px"
																															 :children
																															 [(if path [title :level :level3 :style {:font-weight "bold" :align-self "center" :font-family "Courier New"} :label (str (second (opis-level :1)) " povlači:")])
																																[scroller
																																 :style {:margin-left "auto" :margin-right "auto"}
																																 :v-scroll :auto
																																 :height "100%"
																																 :scroll :auto
																																 :width "99%"
																																 :max-width "1100px"
																																 :min-width "800px"
																																 :max-height "270px"
																																 :min-height "200px"
																																 :child
																																 ;[gap :size "1px"]
																																 (if (not-empty path)
																																	 [data-table (rows-naredbe-stare (rows-level :1) screen) col-widths-types-naredbe-stare (:1 path)]
																																	 [:div ""])]
																																[button
																																 :label [:span "Nova naredba " [:i.zmdi.zmdi-hc-fw-rc.zmdi-collection-plus]]
																																 :disabled? (not= (count path) 1)
																																 :on-click #(add-nova-naredba-event 2)
																																 :style {:color            "white"
																																				 :font-size        "14px"
																																				 ;:background-color (if (:hover? @table-state) "#0072bb" "#4d90fe")
																																				 ;:background-color (if hover? "#0072bb" "#4d90fe")
																																				 :background-color (if (:hover? @showing) "#B5c113" "#B5D113	")
																																				 :font-weight      "100"
																																				 :border           "none"
																																				 :padding          "7px 12px"
																																				 :margin-top       "30px"}
																																 ;:attr {:on-mouse-over (handler-fn (swap! showing assoc-in [:hover?] true))
																																 ;			:on-mouse-out  (handler-fn (swap! showing assoc-in [:hover?] false))}
																																 ]]]]]]

																													 ;[line
																													 ; :size "3px"
																													 ; :color "#BABABA"
																													 ; ;;                                                              :color "#4d90fe"
																													 ; :style {:width "80%" :align-self "center"}]
																													 ;[gap :size "1px"]
																													 :panel-2
																													 [scroller
																														:style {:margin-left "auto" :margin-right "auto" :border-style "solid" :border-color "lightgray" :border-width "0px"}
																														:v-scroll :auto
																														:height "100%"
																														:scroll :auto
																														:width "100%"
																														:max-width "1100px"
																														:min-width "400px"
																														:max-height "600px"
																														:min-height "400px"
																														:child [v-box
																																		:width "100%"
																																		:children [
																																							 (doall (for [level-key (keys (rest path))] (data-tables-level level-key path screen)))
																																							 (if (and (nil? choice) alow-new-veza (not-empty (rest path)))
																																								 [box :child "Dodaj JUS standard u listu veza."
																																									:padding "6px"
																																									:style {:margin-top       "5px"
																																													:margin-bottom    "20px"
																																													:color            "#333"
																																													:background-color "#B5D113"
																																													:border-top       "none"
																																													:border-right     "none"
																																													:border-bottom    "none"
																																													:border-left      "4px solid #B5c113"
																																													:border-radius    "0px"}]
																																								 [box
																																									:align-self :center
																																									:margin "5px"
																																									:child (str choice
																																															" - " (:JUSopis (first (filterv #(= choice (:JUSId %)) jus-all-data))))])
																																							 (if (not-empty (rest path))
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
																																															:label [:span "Dodaj JUS " [:i.zmdi.zmdi-hc-fw-rc.zmdi-download]]
																																															:disabled? (or (not choice) (exist-veza) (not alow-new-veza))
																																															:on-click #(add-veza)
																																															:style {:color            "white"
																																																			:font-size        "14px"
																																																			;:background-color (if (:hover? @table-state) "#0072bb" "#4d90fe")
																																																			;:background-color (if hover? "#0072bb" "#4d90fe")
																																																			:background-color (if (:hover? @showing) "#B5c113" "#B5D113	")
																																																			:font-weight      "100"
																																																			:border           "none"
																																																			:padding          "7px 12px"}
																																															;:attr {:on-mouse-over (handler-fn (swap! showing assoc-in [:hover?] true))
																																															;			 :on-mouse-out  (handler-fn (swap! showing assoc-in [:hover?] false))}
																																															]
																																														 ;:attr {:on-mouse-over (handler-fn (reset! hover? true))
																																														 ;			:on-mouse-out  (handler-fn (reset! hover? false))}]
																																														 ]])
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
																																							 [gap :size "220px"]]]]]
																													[line
																													 :size "3px"
																													 :color "#555657"
																													 :style {:width "90%" :align-self "center"}]
																													[gap :size "20px"]]]]]]
																 [:div "Loading"])))}))


(defn ^:export main []
	;(init-naredbe)
	;(init-veza)
	(init-jus-data)
	(init-only-jus)
	(count-veze-fn)
	(f/set-options!
		{:button-primary {:attrs {:style {:margin "5px"}}}})
	(r/render-component (fn [] [entry-point]) (h/get-elem "app")))





(defn dummy-veza []
	(flatten (mapv (fn [x] (for [child (second x)]
													 (hash-map :Parent (first x) :Child child)))
								 (into [] (zipmap (mapv :JUSId (filter #(= (:Naredba %) 2) (:data @jus-data)))
																	(mapv #(map :JUSId %) (partition 15 (:data @jus-all-data))))))))

(defn prepare-dummy-veza []
	(for [[child parent] (partition 2 (for [veza (select [ALL ALL LAST] (map vec (into #{} (dummy-veza))))] veza))]
		;(do (println parent child)
		(GET "/jus/add-veza" {:params        {:parent parent :child child}
													:handler       #(do (swap! showing assoc-in [:choice] nil) (init-veza [parent child]))
													:error-handler #(do (println "some error occured: " %) (init-veza))})))