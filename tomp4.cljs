(ns tomp4
  (:require [clojure.edn]
            [reagent.core :as r]
            [promesa.core :as p]
            ["node:child_process" :as child-process]
            ["node:path$default" :as path]
            ["ink" :as ink]
            ["ink-task-list" :refer [TaskList Task]]
            ["cli-spinners$default" :as cli-spinners]))

(defn get-frame-count [app-state src-path]
  (let [prom       (p/deferred)
        gfc-cp     (.spawn child-process "ffprobe" #js ["-v" "error" "-select_streams" "v:0" "-count_packets" "-show_entries" "stream=nb_read_packets" "-of" "csv=p=0" src-path] {:shell false})
        out-buffer #js []
        err-buffer #js []
        m (get app-state src-path)
        {:keys [state status frame-total-count]} m]
    (-> gfc-cp
        .-stdout
        (.on "data" (fn [data] (.push out-buffer data))))
    (-> gfc-cp
        .-stderr
        (.on "data" (fn [data] (.push err-buffer data))))
    (.on gfc-cp "close" (fn [code]
                          (if (zero? code)
                            (p/resolve! prom (int (.join out-buffer "")))
                            (p/reject! prom (.join err-buffer "")))))
    prom))

(defn calculate-frame-counts! [app-state]
  (p/loop [[src-path & remaining-src-paths] (-> @app-state :tasks keys)]
    (if src-path
      (p/let [_  (swap! app-state update-in [:tasks src-path] merge {:state "loading" :status "Calculating frame count"})
              fc (-> (get-frame-count app-state src-path)
                     (.then #(swap! app-state update-in [:tasks src-path] merge {:state "pending" :status (str "0/" % " frames") :frame-total-count %}))
                     (.catch #(swap! app-state update-in [:tasks src-path] merge {:state "error" :status %})))]
        (p/recur remaining-src-paths))
      :finished-frame-counting)))

(defn ->app-state [config]
  (r/atom {:done-promise (p/deferred)
           :tasks        (reduce (fn [result [src-path dst-path]]
                                   (let [src-label (.basename path src-path)]
                                     (assoc result src-path {:src-path              src-path
                                                             :dst-path              dst-path
                                                             :src-basename          src-label
                                                             :label                 src-label
                                                             :status                ""
                                                             :state                 "pending"
                                                             :frame-total-count     nil
                                                             :frame-processed-count 0
                                                             :spinner               (.-dots cli-spinners)})))
                                 (sorted-map)
                                 config)}))

(defn task [app-state src-path]
  ^{:key src-path} [:> Task @(r/cursor app-state [:tasks src-path])])

(defn -main [& args]
  (if (-> args count (< 1))
    (println "usage: tomp4 task.edn  (where the edn file contains a map with source path keys and destination path values)")
    (p/let [f         (-> args first nbb.core/slurp)
            config    (clojure.edn/read-string f)
            app-state (->app-state config)
            ink-state (ink/render (r/as-element [:> TaskList (for [src-path (-> @app-state :tasks keys)]
                                                               ^{:key src-path} [task app-state src-path])]))
            _         (calculate-frame-counts! app-state)]
      ((.-unmount ink-state))
      (println "done")
      (js/process.exit 0))))
      

; https://github.com/babashka/nbb/blob/main/examples/ink/select-input.cljs
; https://github.com/vadimdemedes/ink
; https://github.com/privatenumber/ink-task-list
; https://reagent-project.github.io/

