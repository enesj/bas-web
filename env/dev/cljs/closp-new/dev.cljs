(ns closp-new.dev
  (:require [schema.core :as s]
            [jus.table :as table]
            [jus.jus :as jus]))

;(s/set-fn-validation! true)

(enable-console-print!)

(defn main []
  (table/init)
  (jus/main)
  )
