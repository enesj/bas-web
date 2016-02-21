(ns jus.handlers-re-frame
    (:require [re-frame.core :as re-frame]
              [jus.db :as db]))

(re-frame/register-handler
 :initialize-db
 (fn  [_ _]
   db/default-db))
