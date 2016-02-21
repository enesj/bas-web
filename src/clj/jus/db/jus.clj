(ns jus.db.jus
	(:use [com.rpl.specter :only [transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
	(:require [korma.core :refer [select select* where insert delete values update set-fields defentity limit order subselect ]]
						[korma.db :refer [h2 transaction]]
						[cheshire.core :refer [generate-string]]
						[clojure.java.io :as io])
	;(:import (java.util UUID))
	)


(defentity JUS)
(defentity Veza)



(defn update-jus [filter field-data like]
	(let [criteria (if (= like "true") (transform [ALL LAST] #(into [] `(~'like ~(str "%" % "%"))) filter) filter)
				field-data (val (first field-data))]
		(transaction
			(update JUS (set-fields field-data) (where criteria)))))


(defn insert-jus [field-data]
	(let [field-data (val (first field-data))]
					(insert JUS (values field-data))))


(defn delete-jus [jusid]
	(delete JUS (where {:JUSId [= jusid]})))

(defn get-jus-filter [filter like comp]
	(if (> (count filter) 0)
		(let [criteria (if (= like "true") (transform [ALL LAST] #(into [] `(~'like ~(str "%" % "%"))) filter)
																			 (transform [ALL LAST] #(into [] `(~(symbol comp) ~%)) filter))]
			(select JUS (where criteria) (order :JUSId :asc)))
		(select JUS)
		))

(defn naredbe []
	(select JUS (where (> :Naredba  0)) ))

(defn only-jus []
	(select  JUS (where (= :Naredba  0)) ))

;(def only-jus
;	(only-jus-fn))

(defn get-veza []
	(select Veza ))

(defn add-veza [parent child]
	;(println parent child)
	(insert Veza (values {:Parent parent :Child child})))

(defn del-veza [parent child]
	(delete Veza (where {:Parent parent :Child child})))


(defn make-response [status value-map]
	{:status  status
	 :headers {"Content-Type" "application/json"}
	 :body    (generate-string value-map)})



(defn handle-upload [{:keys [filename size tempfile] :as params}]
	(io/copy (io/file tempfile) (io/file (str "./public/pdf/" filename)))
	(if
		(or (not filename)
				(= "" filename))
		(make-response 400 {:status  "ERROR"
												:message "No file parameter sent"})
		(make-response 200 {:status   "OK"
												:filename filename
												:size     (or size 0)
												:tempfile (str tempfile)})))

(defn active-data []
(let [veza (select Veza)]
  (vector (into (naredbe)  (flatten (doall (for [id (partition-all 300
		(doall (into #{} (flatten (mapv vals (select Veza))))))] (filterv #((set id) (:JUSId %)) (only-jus)))))) veza)))

(defn count-veze []
  (let [active-data  (active-data)
        naredbe (naredbe)
        jus-data  (first active-data)
        veza  (second active-data)
        data (into naredbe jus-data)
        ]
    (into {} (mapv #(hash-map (first %) {:total (count (second %)) :locked (count (filterv (fn [x] (= 1 (second x))) (second %))) :childs (map first (second %)) })
                   (transform [ALL LAST ALL] (fn [x] (vector x (:Locked (first (filterv #(= x (:JUSId %)) data)))))
                              (doall (for [id (mapv :JUSId data)] [id (mapv :Child (filterv #(= id (:Parent %)) veza))])))))))


;(defn get-user-by-email [email] (first (select user (where {:email email}) (limit 1))))
;(defn get-user-by-act-id [id] (first (select user (where {:activationid id}) (limit 1))))
;(defn get-user-by-uuid [uuid] (first (select user (where {:uuid uuid}) (limit 1))))
;
;(defn username-exists? [email] (some? (get-user-by-email email)))
;
;(defn create-user [email pw_crypted activationid & [is-active?]]
;  (insert user (values {:email email :pass pw_crypted :activationid activationid :is_active (or is-active? false)
;                         :uuid  (str (UUID/randomUUID))})))
;
;(defn set-user-active [activationid & [active]]
;  (update user (set-fields {:is_active (or active true)}) (where {:activationid activationid})))
;
;(defn update-user [uuid fields] (update user (set-fields fields) (where {:uuid uuid})))
;(defn delete-user [uuid] (delete user (where {:uuid uuid})))
;(defn change-password [email pw] (update user (set-fields {:pass pw}) (where {:email email})))
