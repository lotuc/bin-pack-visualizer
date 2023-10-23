(ns lotuc.binpack.binpack
  (:require
   [cljs.core.async :as a]
   [lotuc.binpack.eb-afit-async :as eb-afit-async]))

(defn calc-progress [statistics]
  (eb-afit-async/calc-progress statistics))

(defn find-best-pack* [input {:keys [on-statistics on-result]}]
  (let [{:keys [res-ch in-ch out-ch]} (eb-afit-async/find-best-pack input)]
    (a/go-loop []
      (let [[_action data :as v] (a/<! out-ch)]
        ;; action is always :pause now.
        ;; decide if continue on the statistics data or your time bounds.
        (when v
          (on-statistics (:statistics data))
          (a/go (a/>! in-ch :continue))
          (recur))))
    (a/go (on-result {:ok (a/<! res-ch)}))))

(defonce binpack-worker (atom nil))
(defonce request-handlers (atom {}))

(defn- on-binpack-worker-message [e]
  (let [[req-id typ data] (js->clj (.. e -data) :keywordize-keys true)
        {:keys [on-statistics on-result] :as h} (get @request-handlers req-id)]
    (when h (cond
              (= typ "statistics") (when on-statistics
                                     (-> data
                                         (update :running-pallet-variants
                                                 (fn [m] (->> m
                                                              (map (fn [[k v]] [(parse-long (name k)) v]))
                                                              (into {}))))
                                         on-statistics))
              (= typ "res") (when on-result (on-result data))))))

(defn init-worker [& {:keys [script-url]}]
  (if-some [w @binpack-worker]
    w
    (let [w (js/Worker. (or script-url "js/binpack_worker.js"))]
      (.. w (addEventListener "message" on-binpack-worker-message))
      (reset! binpack-worker w)
      w)))

(defn- find-best-pack-web-worker* [input {:keys [timeout-ms on-statistics on-result script-url]}]
  (let [w (init-worker {:script-url script-url})
        req-id (str (random-uuid))
        handle-result (fn [r]
                        (swap! request-handlers dissoc req-id)
                        (on-result r))
        clear-timeout (if timeout-ms
                        (let [i (.setTimeout js/window #(handle-result {:err "timeout"}) timeout-ms)]
                          #(.clearTimeout js/window i))
                        (fn []))
        on-result2 (fn [r]
                     (clear-timeout)
                     (handle-result r))]
    (swap! request-handlers assoc req-id {:on-statistics on-statistics :on-result on-result2})
    (.. w (postMessage (clj->js [req-id input])))))

(defn find-best-pack [input {:keys [timeout-ms on-statistics on-result script-url] :as opts}]
  (if (some? (try (init-worker {:script-url script-url})
                  (catch :default _ nil)))
    (find-best-pack-web-worker* input opts)
    (find-best-pack* input opts)))
