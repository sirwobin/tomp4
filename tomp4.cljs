(ns tomp4
  (:require [clojure.edn]
            [reagent.core :as r]
            [promesa.core :as p]
            ["node:child_process" :as child-process]
            ["node:readline$default" :as readline]
            ["node:path$default" :as path]
            ["ink" :as ink]
            ["ink-task-list" :refer [TaskList Task]]
            ["cli-spinners$default" :as cli-spinners]))

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
                                                             :start-time            nil
                                                             :end-time              nil
                                                             :spinner               (.-dots cli-spinners)})))
                                 (sorted-map)
                                 config)}))

(defn get-frame-count [app-state src-path]
  (let [prom           (p/deferred)
        gfc-cp         (.spawn child-process "ffprobe" #js ["-v" "error" "-select_streams" "v:0" "-count_packets" "-show_entries" "stream=nb_read_packets" "-of" "csv=p=0" src-path] {:shell false})
        out-buffer #js []
        err-buffer #js []]
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

(defn convert! [app-state src-path]
  (println "convert staring for" src-path)
  (let [prom          (p/deferred)
        encode-cp     (.spawn child-process "ffmpeg" #js ["-i" src-path "-vcodec" "h264" "-acodec" "mp2" (get-in @app-state [:tasks src-path :dst-path])] {:shell false})
        line-counter  (r/atom 0)
        stdout-lines  (.createInterface readline #js {"input" (.-stdout encode-cp)})
        err-buffer    #js []]
    (.on stdout-lines "line" (fn [line]
                               (let [lc (swap! line-counter inc)]
                                 (println (str lc ": " line)))))
    (-> encode-cp
        .-stderr
        (.on "data" (fn [data] (.push err-buffer data))))
    (.on encode-cp "close" (fn [code]
                              (if (zero? code)
                                (p/resolve! prom :finished-convert)
                                (let [err (.join err-buffer "")]
                                  (swap! app-state update-in [:tasks src-path] merge {:state "error" :status err})
                                  (p/reject! prom err)))))
    prom))

(defn convert-all! [app-state]
  (p/loop [[src-path & remaining-src-paths] (-> @app-state :tasks keys)]
    (cond
      (nil? src-path)
      :finished-frame-counting

      (= (get-in @app-state [:tasks src-path :state]) "error")
      (p/recur remaining-src-paths)

      :else
      (p/let [_               (swap! app-state update-in [:tasks src-path] merge {:state "loading" :status "Calculating frame count"})
              fc-result       (-> (get-frame-count app-state src-path)
                                  (.then  (fn [frame-count]
                                            (swap! app-state update-in [:tasks src-path] merge {:status (str frame-count " frames") :frame-total-count frame-count})
                                            frame-count))
                                  (.catch (fn [error]
                                            (swap! app-state update-in [:tasks src-path] merge {:state "error" :status error})
                                            :error)))
              convert-promise (when-not (= fc-result :error)
                                (-> (convert! app-state src-path)
                                    (.then  #(swap! app-state update-in [:tasks src-path] merge {:state "success" :status (str "todo: calc total time")}))
                                    (.catch #(swap! app-state update-in [:tasks src-path] merge {:state "error" :status %}))))]
        (p/recur remaining-src-paths)))))

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
            _         (convert-all! app-state)]
      ((.-unmount ink-state))
      (println "done")
      (js/process.exit 0))))
      

; https://nodejs.org/api/readline.html
; https://stackoverflow.com/a/54640326 <- readline example
; https://github.com/babashka/nbb/blob/main/examples/ink/select-input.cljs
; https://github.com/vadimdemedes/ink
; https://github.com/privatenumber/ink-task-list
; https://reagent-project.github.io/

