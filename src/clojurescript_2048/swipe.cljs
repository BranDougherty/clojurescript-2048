(ns clojurescript-2048.swipe)

; The starting point of the swipe event.
(def initial-touch (atom nil))

; The distance in pixels required to trigger a swipe event.
; If 30 pixels is good enough for JQuery it'll probably be
; fine for us.
(def SWIPE-DISTANCE 30)

(defn distance
  [x1 y1 x2 y2]
  (let [a (- x1 x2)
        b (- y1 y2)]
    (Math/sqrt (+ (* a a) (* b b)))))

(defn greatest-direction
  [x1 y1 x2 y2]
  (let [xdist (- x1 x2)
        ydist (- y1 y2)]
    (if (> (Math/abs xdist) (Math/abs ydist))
      (if (pos? xdist)
        :left
        :right)
      (if (pos? ydist)
        :up
        :down))))

(defn handle-touch-start
  [e]
  (.preventDefault e)
  (when (nil? @initial-touch)
    (reset! initial-touch
            (.item (.-changedTouches e) 0))))

(defn handle-touch-cancel
  [e]
  (.preventDefault e)
  (let [it @initial-touch
        changed-touches (.-changedTouches e)
        length (.-length changed-touches)]
    (when-not (nil? it)
      (loop [i 0]
        (when-not (= i length)
          (if-not (= (.-identifer it) (.-identifer (.item changed-touches 0)))
            (recur (+ i 1))
            (reset! initial-touch nil)))))))

(defn handle-swipe
  [swipe-fn]
  (fn [start-touch end-touch]
    (when (>= (distance (.-clientX start-touch) (.-clientY start-touch)
                        (.-clientX end-touch) (.-clientY end-touch))
              SWIPE-DISTANCE)
      (swipe-fn (greatest-direction (.-clientX start-touch) (.-clientY start-touch)
                                                        (.-clientX end-touch) (.-clientY end-touch))))
    (reset! initial-touch nil)))

(defn handle-touch-end
  [swipe-fn]
  (let [swipe-handler (handle-swipe swipe-fn)]
    (fn [e]
      (.preventDefault e)
      (let [it @initial-touch
            changed-touches (.-changedTouches e)
            length (.-length changed-touches)]
        (when-not (nil? it)
          (loop [i 0]
            (when-not (= i length)
              (if-not (= (.-identifer it) (.-identifer (.item changed-touches i)))
                (recur (inc i))
                (swipe-handler it (.item changed-touches i))))))))))

(defn init-listeners
  [id swipe-fn]
  (let [ele (.getElementById js/document id)]
  (.addEventListener ele "touchstart" handle-touch-start false)
  (.addEventListener ele "touchend" (handle-touch-end swipe-fn) false)
  (.addEventListener ele "touchcancel" handle-touch-cancel false)))

