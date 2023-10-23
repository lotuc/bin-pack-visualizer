(ns lotuc.build
  (:require
   [shadow.html]))

(defn- prepare-html [build-state & _]
  (shadow.html/copy-file build-state "src/html/index.html" "public/index.html"))

(defn prepare-static-files
  {:shadow.build/stage :flush}
  [{:shadow.build/keys [build-id mode stage config]
    :as build-state}
   & _args]
  (doto build-state
    (prepare-html)))
