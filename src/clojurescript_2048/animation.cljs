(ns clojurescript-2048.animation
  (:require [clojurescript-2048.js-util :as jsu]
            [goog.string :as gs]
            [goog.string.format]))

; The list of currently running animations.
(def animations (atom []))

; The amount of time in milliseconds at which to finish animations.
(def ANIMATION-STEP-TIME 16)
; Length in milliseconds for each animation to take.
(def ANIMATION-LENGTH 200)

(defn space-sep
  [coll]
  (apply str (interpose " " coll)))

(defn pos-to-prop-val
  "Returns appropriate value for left/top property of a game tile in the
  nth row/column."
  [n]
  (* 100 n))

(defn no-tween
  [percent-elapsed start end]
  start)

(defn linear-tween
  [percent-elapsed start end]
  (+ start (* (- end start) percent-elapsed)))

(defn quadratic-tween
  [percent-elapsed start end]
  (if (= percent-elapsed 100)
    end
    (+ start (* (- end start) (Math/pow percent-elapsed 2)))))

(defn transformation-value
  [transformation percent-elapsed]
  (let [new-value (gs/format "%f" ((:tween-function transformation)
                                              percent-elapsed
                                              (:initial-value transformation)
                                              (:target-value transformation)))]
    (str (:function transformation) "(" new-value (:unit transformation) ")")))

(defn finished-transformation-value
  [transformation]
  (let [final-value (gs/format "%f" (:target-value transformation))]
    (str (:function transformation) "(" final-value (:unit transformation) ")")))

(defn process-transformations
  [ele transformations percent-elapsed]
  (let [transform-prop-value (space-sep
                               (doall (map #(transformation-value %1 percent-elapsed)
                                    transformations)))]
    (jsu/set-style ele "transform" transform-prop-value)))

(defn finish-transformations
  [ele transformations]
  (let [transform-prop-value (space-sep
                               (doall (map #(finished-transformation-value %1)
                                           transformations)))]
    (jsu/set-style ele "transform" transform-prop-value)))

(defn add-animation
  [animation]
  (when-not (nil? (:transitions animation))
    (doall (map (fn [transition]
                  (let [new-prop-val (str (:initial-value transition) (:unit transition))]
                    (jsu/set-style (:ele animation) (:property transition) new-prop-val)))
                (:transitions animation))))
  (when-not (nil? (:transformations animation))
    (process-transformations (:ele animation) (:transformations animation) 0))
  (let [new-anim (assoc animation :started (.now js/Date))]
    (swap! animations #(conj %1 new-anim))))

(defn finish-animation
  [animation]
  (let [ele (:ele animation)]
    (when-not (nil? (:transitions animation))
      (doall (map (fn [transition]
                    (let [new-prop-val (str (:target-value transition) (:unit transition))]
                    (when-not (nil? ele)
                      (jsu/set-style ele (:property transition) new-prop-val))))
                  (:transitions animation))))
    (when-not (nil? (:transformations animation))
      (finish-transformations ele (:transformations animation)))
    (when-not (nil? (:anim-end-callback animation))
      ((:anim-end-callback animation) animation))
    nil))

(defn remove-finished-animations
  []
  (swap! animations (fn [old-animations]
                      (filter #(not (nil? %1)) old-animations))))

(defn process-transition
  [ele transition percent-elapsed]
  (let [new-value ((:tween-function transition)
                   percent-elapsed
                   (:initial-value transition)
                   (:target-value transition))
        new-prop-value (str new-value (:unit transition))]
    (jsu/set-style ele (:property transition) new-prop-value)))

(defn process-animation
  [animation]
  (let [time-elapsed (- (.now js/Date) (:started animation))
        time-left (- (:duration animation) time-elapsed)
        percent-elapsed (/ time-elapsed (:duration animation))]
    (if (< time-left ANIMATION-STEP-TIME)
      (finish-animation animation)
      (do
        (when-not (nil? (:transitions animation))
          (doall (map #(process-transition (:ele animation) %1 percent-elapsed)
                      (:transitions animation))))
        (when-not (nil? (:transformations animation))
          (process-transformations (:ele animation) (:transformations animation) percent-elapsed))
        animation))))

(defn finish-all-animations
  []
  (loop []
    (let [old-animations @animations
          not-done (filter #(not (nil? %1)) old-animations)]
      (reset! animations [])
      (let [processed (into [] (map finish-animation not-done))]
        (swap! animations (fn [old-animations] (into old-animations processed))))
      (remove-finished-animations))
    (when-not (empty? @animations)
      (recur))))

(defn process-animations
  []
  (let [old-animations @animations
        not-done (filter #(not (zero? (:duration %1))) old-animations)]
    (reset! animations [])
    (let [processed (into [] (map process-animation not-done))]
      (swap! animations (fn [old-animations] (into old-animations processed))))
    (remove-finished-animations)
    (js/requestAnimationFrame process-animations)))

(defn anim-to-pos
  [ele start-row end-row start-col end-col anim-end-cb]
  (add-animation {
                  :transitions
                  [
                   {
                    :property "left"
                    :unit "%"
                    :initial-value (pos-to-prop-val start-col)
                    :target-value (pos-to-prop-val end-col)
                    :tween-function quadratic-tween
                    }
                   {
                    :property "top"
                    :unit "%"
                    :initial-value (pos-to-prop-val start-row)
                    :target-value (pos-to-prop-val end-row)
                    :tween-function quadratic-tween
                    }
                   ]
                  :ele ele
                  :duration (* ANIMATION-LENGTH .65)
                  :anim-end-callback anim-end-cb
                  }))

(defn anim-shrink
  [ele row col value]
  (let [tx (pos-to-prop-val col)
        ty (pos-to-prop-val row)
        anim-len (* .5 ANIMATION-LENGTH)]
    (add-animation {
                    :transformations
                    [
                     {
                      :function "scale"
                      :unit ""
                      :initial-value 1.25
                      :target-value 1
                      :tween-function linear-tween
                      }
                     ]
                    :ele ele
                    :duration anim-len
                    })))

(defn anim-pop
  [ele row col value]
  (let [tx (pos-to-prop-val col)
        ty (pos-to-prop-val row)
        anim-len (* .5 ANIMATION-LENGTH)]
    (add-animation {
                    :transformations
                    [
                     {
                      :function "scale"
                      :unit ""
                      :initial-value 1
                      :target-value 1.25
                      :tween-function quadratic-tween
                      }
                     ]
                    :ele ele
                    :duration anim-len
                    :anim-end-callback #(anim-shrink ele row col value)
                    })))

(defn anim-into
  [ele start-row end-row start-col end-col value]
  (let [anim-end-cb (fn [anim]
                      (let [text-node (.createTextNode js/document (str value))
                            div-node (.-firstChild ele)]
                        (when-not (nil? div-node)
                          (jsu/remove-all-children div-node)
                          (.appendChild div-node text-node)
                          (if (<= value 2048)
                            (goog.object/set div-node "className" (str "tile tile-" value))
                            (goog.object/set div-node "className" "tile tile-big"))
                          (anim-pop div-node end-row end-col value))))]
    (anim-to-pos ele start-row end-row start-col end-col anim-end-cb)))

(defn anim-merged
  [ele start-row end-row start-col end-col]
  (let [anim-end-cb (fn [anim]
                      (when-not (nil? (:ele anim))
                        (.remove (:ele anim))))]
    (anim-to-pos ele start-row end-row start-col end-col anim-end-cb)))

(defn anim-shift
  [ele start-row end-row start-col end-col]
  (anim-to-pos ele start-row end-row start-col end-col nil))

(defn anim-grow
  [ele row col value]
  (let [div-node (.-firstChild ele)
        tx (pos-to-prop-val col)
        ty (pos-to-prop-val row)]
    (jsu/set-style ele "left" (str tx "%"))
    (jsu/set-style ele "top" (str ty "%"))
    (add-animation {
                    :transformations
                    [
                     {
                      :function "scale"
                      :unit ""
                      :initial-value 0
                      :target-value 1
                      :tween-function linear-tween
                      }
                     ]
                    :ele div-node
                    :duration ANIMATION-LENGTH
                    })))

(defn add-tile
  [id row column value]
  (let [tile-root (.getElementById js/document "tile-root")
        container-node (.createElement js/document "div")
        div-node (.createElement js/document "div")
        text-node (.createTextNode js/document (str value))
        new-id (str "tile-" id)]
    (goog.object/set container-node "className" "tile-space")
    (goog.object/set container-node "id" new-id)
    (if (<= value 2048)
      (goog.object/set div-node "className" (str "tile tile-" value))
      (goog.object/set div-node "className" "tile tile-big"))
    (.appendChild div-node text-node)
    (.appendChild container-node div-node)
    (.appendChild tile-root container-node)
    (anim-grow container-node row column value)))

(defn anim-turn-score
  [turn-score]
  (when-not (zero? turn-score)
    (let [ele (.getElementById js/document "turn-score")]
      (goog.object/set ele "innerHTML" (str "+" turn-score))
      (add-animation {
                      :transitions
                      [
                       {
                        :property "top"
                        :unit "%"
                        :initial-value -50
                        :target-value -200
                        :tween-function linear-tween
                        }
                       {
                        :property "opacity"
                        :unit ""
                        :initial-value 1.0
                        :target-value 0.0
                        :tween-function linear-tween
                        }
                       ]
                      :ele ele
                      :duration ANIMATION-LENGTH
                      }))))

(defn init
  []
    (js/requestAnimationFrame process-animations))

