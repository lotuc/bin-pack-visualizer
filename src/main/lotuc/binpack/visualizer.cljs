(ns lotuc.binpack.visualizer
  (:require
   ["@react-three/drei" :refer [Bounds Edges Environment GizmoHelper
                                GizmoHelper Grid OrbitControls PivotControls]]
   ["@react-three/fiber" :refer [Canvas useFrame]]
   ["leva" :refer [useControls]]
   ["react" :refer [useRef]]
   ["react-dom/client" :refer [createRoot]]
   [clojure.string :as string]
   [goog.dom :as gdom]
   [lotuc.binpack.binpack :as binpack]
   [lotuc.binpack.colors :refer [seed-rand-color]]
   [lotuc.binpack.eb-afit-io :as eb-afit-io]
   [reagent.core :as r]))

;; more test inputs here:
;; https://github.com/wknechtel/3d-bin-pack/tree/master/test
(def sample-input-txt "10, 10, 10
1. 5, 5, 5, 8")
(def sample-visualdot-txt "104   96   84
  0    0    0  104   24   70
  0    0   70  104   48   14
  0   24    0  104   24   70
  0   48    0  104   24   70
  0   48   70  104   48   14
  0   72    0  104   24   70")

(defn ItemBox [_props]
  (let [hovered (r/atom false)]
    (fn [{:keys [position geometry color opacity]}]
      (let [mesh (useRef)
            _ (useFrame (fn [_state _delta]))]
        [:mesh {:ref mesh
                :position (clj->js position)
                :onPointerOver (fn [_event] (reset! hovered true))
                :onPointerOut (fn [_event] (reset! hovered false))}
         [:boxGeometry {:args (clj->js geometry)}]
         [:meshBasicMaterial
          {:transparent true :opacity opacity :depthTest false :color color}]
         [:> Edges
          [:meshBasicMaterial {:color color :depthTest false}]]]))))

(defn PalletBox [_props]
  (fn [{:keys [position geometry color opacity]}]
    (let [mesh (useRef)
          _ (useFrame (fn [_state _delta]))]
      [:> Bounds {:fit true :clip true :observe true :dmaping 6 :margin 2}
       [:mesh {:ref mesh
               :position (clj->js position)}
        [:boxGeometry {:args (clj->js geometry)}]
        [:meshBasicMaterial {:transparent true :opacity 0}]
        [:> Edges {:scale 1.01 :lineWidth 3}
         [:lineBasicMaterial {:color color}]]]])))

(def default-control-options
  {:gridSize [10 10]
   :boxOpacity {"value" 0.7 "min" 0 "max" 1 "step" 0.1}

   :cellSize {"value" 1 "min" 0 "max" 10 "step" 1}
   :cellThickness {"value" 1 "min" 0 "max" 5 "step" 0.1}
   :cellColor "#6f6f6f"
   :sectionThickness {"value" 1 "min" 0 "max" 5 "step" 0.1}
   :followCamera true
   :infiniteGrid true})

(defn- useControls' [folder-name args]
  (let [args' (->> args
                   (map (fn [[k v]] [k (clj->js v)]))
                   (into {})
                   clj->js)]
    (js->clj (useControls folder-name args'))))

(defn box-visualizer [_props]
  (fn [{:keys [packed-res]}]
    (let [packed-boxes (:pack packed-res)
          pallet-variant (:pallet-variant packed-res)

          {:strs [packedBoxes] :as c1}
          (useControls'
           "Replay" {:packedBoxes {"value" -1 "min" -1 "step" 1}})

          {:strs [gridSize boxOpacity] :as c}
          (useControls'
           "Display" default-control-options)

          grid-props
          (dissoc c "boxOpacity" "gridSize")

          trans-pos
          (fn [pos geo]
            (map + pos (map #(/ % 2) geo)))]
      [:> Canvas
       (when pallet-variant
         [:f> PalletBox {:position (trans-pos [0 0 0] pallet-variant)
                         :geometry pallet-variant
                         :color "red"}])
       [:<>
        (let [rand-color (seed-rand-color 42)]
          (->> packed-boxes
               (map-indexed (fn [i v] [i v]))
               (filter (fn [[i _v]] (or (< packedBoxes 0) (< i packedBoxes))))
               (map (fn [[i {:keys [pack-dims pack-coord]}]]
                      [:f> ItemBox
                       {:key i
                        :position (trans-pos pack-coord pack-dims)
                        :geometry pack-dims
                        :opacity boxOpacity
                        :color (rand-color)}]))))]

       [:> Grid (merge {:position #js [0 0 0] :args gridSize} grid-props)]
       [:> Environment {:preset "city"}]
       [:> OrbitControls {:makeDefault true}]
       [:> PivotControls]
       [:> GizmoHelper {:makeDefault true :alignment "bottom-right" :margin #js [80 80]}
        [:> GizmoHelper {:axisColors #js ["#9d4b4b", "#2f7f4f", "#3b5b9d"] :labelColor "white"}]]])))

(defn build-console-txt [{:keys [pallet-variants running-pallet-variants] :as statistics} elapsed]
  (when statistics
   (with-out-str
     (println "Total variants:" (count pallet-variants))
     (println "Elapsed Millis:" elapsed)
     (println "Running variants:")
     (doseq [[variant-num {:keys [running-layer-thicknesses finished-layer-thicknesses total-layer-thicknesses]}] running-pallet-variants]
       (println "- Variant: " (get pallet-variants variant-num))
       (when running-layer-thicknesses
        (println "  Iterating Layer Thickness:" running-layer-thicknesses))
       (println "  Finished Iteration:" (str (or finished-layer-thicknesses 0) "/" (or total-layer-thicknesses 0)))))))

(defn app
  []
  (let [eb-afit-txt (r/atom sample-input-txt)
        eb-afit-visualdot-txt (r/atom sample-visualdot-txt)
        console-txt (r/atom nil)
        txt-type (r/atom "eb-afit-input")
        packed-res (r/atom nil)
        calculating (r/atom false)
        calculating-progress (r/atom 0)

        find-best-pack
        (fn [txt]
          (reset! calculating true)
          (reset! calculating-progress nil)
          (reset! packed-res nil)
          (reset! console-txt nil)

          (let [i (eb-afit-io/read-input txt)
                t0 (js/Date.)]
            (binpack/find-best-pack
             i
             {:on-statistics (fn [p]
                               (reset! calculating-progress (/ (Math/floor (* 100 (binpack/calc-progress p))) 100.0))
                               (reset! console-txt (build-console-txt p (- (js/Date.) t0))))
              :on-result (fn [{:keys [err ok]}]
                           (reset! calculating false)
                           (reset! calculating-progress 100)
                           (let [t1 (js/Date.)]
                             (->> (into [(str "ELAPSED TIME: " (- t1 t0) "msec")
                                         (str "PALLET ORIENTATION: " (seq (:pallet-variant ok)))
                                         (str "PALLET VOLUME: " (:pallet-volume i))
                                         (str "PERCENTAGE OF PALLET VOLUME USED: " (:percentage-used ok) "%")
                                         (str "PACKED NUMBER OF BOXES: " (:packed-number ok))
                                         (str "TOTAL NUMBER OF BOXES: " (count (:boxes i)))
                                         (str "FIRST LAYER THICKNESS: " (:first-layer-thickness ok))]
                                        (when-some [b (seq (:unpacked ok))]
                                          (->> b
                                               (map (fn [{:keys [label dims]}] (str " " label " " dims)))
                                               (into ["UNPACKED BOXES:"]))))
                                  (string/join "\n")
                                  (reset! console-txt)))
                           (reset! packed-res ok))})))

        render-visualdot
        (fn [txt]
          (let [{:keys [pallet pack]} (eb-afit-io/read-visualdot txt)]
            (reset! packed-res {:pallet-variant pallet :pack pack})))

        on-textrea-change
        (fn [e]
          (reset! (if (= @txt-type "eb-afit-input")
                    eb-afit-txt eb-afit-visualdot-txt)
                  (.. e -target -value)))

        on-button-click
        (fn [_]
          (if (= @txt-type "eb-afit-input")
            (find-best-pack @eb-afit-txt)
            (render-visualdot @eb-afit-visualdot-txt)))]

    ;; initialize with sample input's packing found.
    (find-best-pack @eb-afit-txt)

    (fn []
      (let [textarea-txt
            (if (= @txt-type "eb-afit-input")
              @eb-afit-txt @eb-afit-visualdot-txt)]
        [:div {:class "h-screen flex flex-col"}
         [:div {:class "ml-2"}
          [:a {:href "https://github.com/lotuc/bin-pack"
               :target "_blank"
               :class "font-semibold"} "Github lotuc/bin-pack"]]
         [:div {:class "h-full p-2 flex flex-row"}
          ;; Left side input area
          [:div {:class "w-1/4 h-full flex flex-col mr-2 space-y-2"}
           [:textarea {:class "grow border-solid p-1 border-2 border-indigo-600"
                       :value textarea-txt
                       :onChange on-textrea-change}]
           [:select {:class "border-solid border-2"
                     :value @txt-type
                     :onChange (fn [e] (reset! txt-type (.. e -target -value)))}
            [:option {:value "eb-afit-input"} "eb-afit input"]
            [:option {:value "eb-afit-visualdot"} "eb-afit visualdot"]]

           [:button {:class (str "p-1 w-full bg-violet-500 hover:bg-violet-600"
                                 " active:bg-violet-700 focus:outline-none"
                                 " focus:ring focus:ring-violet-300 text-white")
                     :disabled @calculating
                     :onClick on-button-click}
            (if (= @txt-type "eb-afit-input")
              (str "Find best pack" (when @calculating (str " (calculating " @calculating-progress "%)")))
              "Render boxes")]
           [:pre {:class "h-32 p-1 border-2 border-indigo-600 text-sm overflow-scroll text-emerald-600"}
            @console-txt]]

          ;; Right side rendering area
          [:div {:class "w-3/4 h-full border-solid border-2 border-indigo-600"}
           [:f> box-visualizer {:packed-res @packed-res}]]]]))))

(defonce root (createRoot (gdom/getElement "root")))

;; https://stackoverflow.com/questions/72389560/how-to-rerender-reagent-ui-with-react-18-and-shadow-cljs-reload
(defn init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (.render root (r/as-element [app])))

(defn ^:dev/after-load start []
  (init))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))
