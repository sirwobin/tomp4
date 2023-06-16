(ns tomp4
  (:require [clojure.edn]
            [clojure.string :as string]
            [nbb.core]
            [reagent.core :as r]
            [promesa.core :as p]
            ["node:child_process" :as child-process]
            ["node:readline$default" :as readline]
            ["node:path$default" :as path]
            ["node:util$default" :as util]
            ["ink" :as ink]
            ["ink-task-list" :refer [TaskList Task]]
            ["cli-spinners$default" :as cli-spinners]))

(defn edn->app-state [config]
  (->> config
       (reduce (fn [result [src-path dst-path]]
                 (let [src-label (.basename path src-path)]
                   (assoc result src-path {:src-path              src-path
                                           :dst-path              dst-path
                                           :label                 src-label
                                           :status                ""
                                           :state                 "pending"
                                           :frame-total-count     nil
                                           :last-ten-fps          (list)
                                           :spinner               (.-dots cli-spinners)})))
               (sorted-map))
       r/atom))

(defn get-frame-count [src-path]
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

(defn add-non-zero-then-keep-last-ten [coll v]
  (if-not (or (js/isNaN v) (zero? v))
    (->> (conj coll v)
         (take 10))
    coll))

(defn zero-pad [n]
  (cond
    (< 9 n)
    (str n)

    (< 0 n)
    (str "0" n)

    :else
    "00"))

(defn seconds->human-readable [s]
  (let [hours             (int (/ s 3600))
        remaining-seconds (if (< 0 hours)
                            (- s (* hours 3600))
                            s)
        minutes           (int (/ remaining-seconds 60))
        seconds           (if (< 0 minutes)
                            (- (* minutes 60) remaining-seconds)
                            remaining-seconds)]
    (cond
      (< 0 hours)
      (.format util "%s:%s:%s" (zero-pad hours) (zero-pad minutes) (zero-pad seconds))

      (< 0 minutes)
      (.format util "%s:%s" (zero-pad minutes) (zero-pad seconds))

      :else
      (str seconds " seconds"))))

(defn convert-status-update [{:keys [last-ten-fps frame-total-count] :as task-map} frame-processed-count]
  (if (< 2 (count last-ten-fps))
    (let [average-fps      (/ (reduce + last-ten-fps) (count last-ten-fps))
          remaining-frames (- frame-total-count frame-processed-count)
          percent-done     (.toFixed (* (/ frame-processed-count frame-total-count) 100) 1)
          eta              (-> (/ remaining-frames average-fps)
                               int
                               seconds->human-readable)]
      (assoc task-map :status (.format util "ETA %s, %f%% complete, %f average FPS" eta percent-done (.toFixed average-fps 1))))
    (assoc task-map :status (.format util "%d/%d frames processed" frame-processed-count frame-total-count))))

(defn convert! [app-state src-path]
  (let [prom          (p/deferred)
        encode-cp     (.spawn child-process "ffmpeg" #js ["-i" src-path "-vcodec" "h264" "-acodec" "mp2" "-n" "-progress" "-" "-v" "error" (get-in @app-state [src-path :dst-path])] {:shell false})
        stdout-lines  (.createInterface readline #js {"input" (.-stdout encode-cp)})
        err-buffer    #js []]
    (.on stdout-lines "line" (fn [line]
                               (cond
                                 (string/starts-with? line "fps=")
                                 (let [fps (-> line
                                               (subs 4)
                                               js/parseFloat)]
                                   (swap! app-state update-in [src-path :last-ten-fps] add-non-zero-then-keep-last-ten fps))

                                 (string/starts-with? line "frame=")
                                 (let [frame (-> line
                                                 (subs 6)
                                                 js/parseInt)]
                                   (swap! app-state update src-path convert-status-update frame)))))
    (-> encode-cp
        .-stderr
        (.on "data" (fn [data] (.push err-buffer data))))
    (.on encode-cp "close" (fn [code]
                              (if (zero? code)
                                (p/resolve! prom :finished-convert)
                                (let [err (-> err-buffer
                                              (.join "")
                                              string/trim)]
                                  (swap! app-state update src-path merge {:state "error" :status err})
                                  (p/reject! prom err)))))
    prom))

(defn convert-all! [app-state]
  (p/loop [[src-path & remaining-src-paths] (keys @app-state)]
    (cond
      (nil? src-path)
      :finished-convert-all

      (= (get-in @app-state [src-path :state]) "error")
      (p/recur remaining-src-paths)

      :else
      (p/let [_               (swap! app-state update src-path merge {:state "loading" :status "Calculating frame count"})
              fc-result       (-> (get-frame-count src-path)
                                  (.then  (fn [frame-count]
                                            (swap! app-state update src-path merge {:status (str frame-count " frames") :frame-total-count frame-count})
                                            frame-count))
                                  (.catch (fn [error]
                                            (swap! app-state update src-path merge {:state "error" :status error})
                                            :error)))
              convert-promise (when-not (= fc-result :error)
                                (-> (convert! app-state src-path)
                                    (.then  #(swap! app-state update src-path merge {:state "success" :status (str "Finished at " (js/Date.))}))
                                    (.catch #(swap! app-state update src-path merge {:state "error" :status %}))))]
        (p/recur remaining-src-paths)))))

(defn task [app-state src-path]
  ^{:key src-path} [:> Task @(r/cursor app-state [src-path])])

(defn -main [& args]
  (if (-> args count (< 1))
    (println "usage: tomp4 task.edn  (where the edn file contains a map with source path keys and destination path values)")
    (p/let [tasks-str (-> args first nbb.core/slurp)
            app-state (-> tasks-str clojure.edn/read-string edn->app-state)
            ink-state (ink/render (r/as-element [:> TaskList (for [src-path (keys @app-state)]
                                                               ^{:key src-path} [task app-state src-path])]))
            _         (convert-all! app-state)]
      ((.-unmount ink-state))
      (js/process.exit 0))))

