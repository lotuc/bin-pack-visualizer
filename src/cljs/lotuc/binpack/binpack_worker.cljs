(ns lotuc.binpack.binpack-worker
  (:require
   [lotuc.binpack.binpack :as binpack]))

(defn init []
  (js/self.addEventListener
   "message"
   (fn [^js e]
     (let [[req-id params] (js->clj (.. e -data) :keywordize-keys true)
           send! (fn [typ d] (js/postMessage (clj->js [req-id typ d])))]
       (binpack/find-best-pack*
        params
        {:on-statistics (fn [p] (send! "statistics" p) 0)
         :on-result (fn [v] (send! "res" v))})))))
