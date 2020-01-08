(ns clojurescript-2048.core
  (:require [clojure.set :as set]
            [cljs.reader :as reader]
            [clojurescript-2048.animation :as anim]
            [clojurescript-2048.game-logic :as game-logic]
            [clojurescript-2048.swipe :as swipe]
            [clojurescript-2048.js-util :as jsu]))

(def keycode-to-command {
                           37 :left  ; left arrow
                           65 :left  ; a
                           38 :up    ; up arrow
                           87 :up    ; w
                           39 :right ; right arrow
                           68 :right ; d
                           40 :down  ; down arrow
                           83 :down  ; s
                           82 :reset ; r
                           })

(defn is-command
  [keycode]
  (contains? (set (keys keycode-to-command)) keycode))

(defn update-score
  [state]
  (let [score-ele (.getElementById js/document "total-score")
        high-score-ele (.getElementById js/document "high-score")]
    (goog.object/set score-ele "innerHTML" (str (:score @state)))
    (goog.object/set high-score-ele "innerHTML" (str (:high-score @state)))))

(defn write-state
  [state]
  (.setItem (.-localStorage js/window) "state" (pr-str state)))

(defn get-state
  []
  (.getItem (.-localStorage js/window) "state"))

(defn animate-new-tiles
  [id-board new-tiles]
  (doall (map (fn [id]
                (let [[row col tile] (id-board id)]
                  (anim/add-tile id row col (:value tile))))
              new-tiles)))

(defn animate-merges
  [old-id-board new-id-board merges]
  (doall (map (fn [{destroyed-tile :destroyed
                    into-tile :into}]
                (let [destroyed-id (:id destroyed-tile)
                      into-id (:id into-tile)
                      [destroyed-row destroyed-col] (old-id-board destroyed-id)
                      [into-old-row into-old-col] (old-id-board into-id)
                      [into-new-row into-new-col] (new-id-board into-id)
                      new-destroyed-id (str "tile-" destroyed-id)
                      new-into-id (str "tile-" into-id)
                      new-destroyed-ele (.getElementById js/document new-destroyed-id)
                      new-into-ele (.getElementById js/document new-into-id)]
                  (anim/anim-merged new-destroyed-ele destroyed-row into-new-row destroyed-col into-new-col)
                  (anim/anim-into new-into-ele into-old-row into-new-row into-old-col into-new-col (* 2 (:value into-tile)))))
              merges)))

(defn animate-shifts
  [old-id-board new-id-board shifted]
  (doall (map (fn [id]
                (let [[old-row old-col old-tile] (old-id-board id)
                      [new-row new-col new-tile] (new-id-board id)
                      new-value (:value new-tile)
                      new-id (str "tile-" id)
                      new-ele (.getElementById js/document new-id)]
                  (anim/anim-shift new-ele old-row new-row old-col new-col)))
              shifted)))

(defn animate-turn
  [old-board new-board merges]
  (anim/finish-all-animations)
  (let [old-id-board (game-logic/id-mapped-board old-board)
        new-id-board (game-logic/id-mapped-board new-board)
        old-keys (set (keys old-id-board))
        new-keys (set (keys new-id-board))
        intos (set (map #(get-in %1 [:into :id]) merges))
        new-tiles (filter #(not (nil? %1)) (set/difference new-keys old-keys))
        shifted (filter #(not (nil? %1)) (set/difference (set/intersection new-keys old-keys) intos))]
    (animate-new-tiles new-id-board new-tiles)
    (animate-shifts old-id-board new-id-board shifted)
    (animate-merges old-id-board new-id-board merges)))

(defn display-losing-notif
  []
  (let [ele (.getElementById js/document "lost-notif")]
    (js/setTimeout (fn []
                     (when (= (:state @game-logic/game-state) :lost)
                       (jsu/set-style ele "visibility" "visible")
                       (anim/add-animation {
                                            :transitions
                                            [
                                             {
                                              :property "opacity"
                                              :unit ""
                                              :initial-value 0.0
                                              :target-value 1.0
                                              :tween-function anim/linear-tween
                                              }
                                             ]
                                            :ele ele
                                            :duration anim/ANIMATION-LENGTH
                                            }))) 1000)))

(defn display-winning-notif
  []
  (let [ele (.getElementById js/document "won-notif")]
    (js/setTimeout (fn []
                     (when (= (:state @game-logic/game-state) :won)
                       (jsu/set-style ele "visibility" "visible")
                       (anim/add-animation {
                                            :transitions
                                            [
                                             {
                                              :property "opacity"
                                              :unit ""
                                              :initial-value 0.0
                                              :target-value 1.0
                                              :duration anim/ANIMATION-LENGTH
                                              :tween-function anim/linear-tween
                                              }
                                             ]
                                            :ele ele
                                            :duration anim/ANIMATION-LENGTH
                                            }))) 1000)))


(defn hide-losing-notif
  []
  (let [ele (.getElementById js/document "lost-notif")]
    (jsu/set-style ele "visibility" "hidden")
    (jsu/set-style ele "opacity" "0.0")))

(defn hide-winning-notif
  []
  (let [ele (.getElementById js/document "won-notif")]
    (jsu/set-style ele "visibility" "hidden")
    (jsu/set-style ele "opacity" "0.0")))

(defn keep-going
  []
  (hide-winning-notif)
  (swap! game-logic/game-state (fn [old-state]
                                 (assoc old-state :state :playing))))

(defn do-move
  [direction]
  (when (= (:state @game-logic/game-state) :playing)
    (let [old-board (:board @game-logic/game-state)
          [new-board merges] (game-logic/move old-board direction)
          turn-score (game-logic/turn-score merges)
          old-score (:score @game-logic/game-state)
          new-score (+ turn-score old-score)]
      (animate-turn old-board new-board merges)
      (anim/anim-turn-score turn-score)
      (swap! game-logic/game-state (fn [old-state]
                                     (assoc old-state
                                            :board new-board
                                            :score new-score)))
      (when (> new-score (:high-score @game-logic/game-state))
        (swap! game-logic/game-state (fn [old-state]
                                       (assoc old-state :high-score new-score))))
      (update-score game-logic/game-state)
      (if (game-logic/lost? new-board)
        (do
          (game-logic/reset-state game-logic/game-state)
          (write-state @game-logic/game-state)
          (display-losing-notif)
          (swap! game-logic/game-state (fn [old-state]
                                         (assoc old-state :state :lost))))
        (do
          (when (game-logic/won? new-board)
            (when (:waiting-for-win @game-logic/game-state)
              (display-winning-notif)
              (swap! game-logic/game-state (fn [old-state]
                                             (assoc old-state :state :won
                                                    :waiting-for-win false)))))

          (write-state @game-logic/game-state))))))

(defn reset
  []
  (hide-losing-notif)
  (hide-winning-notif)
  (anim/finish-all-animations)
  (jsu/remove-all-children (.getElementById js/document "tile-root"))
  (game-logic/reset-state game-logic/game-state)
  (update-score game-logic/game-state)
  (animate-turn game-logic/empty-board (:board @game-logic/game-state) [])
  (swap! game-logic/game-state (fn [old-state]
                                 (assoc old-state :state :playing)))
  (write-state @game-logic/game-state))

(defn process-command
  [command]
  (case command
    :left (do-move :left)
    :up (do-move :up)
    :right (do-move :right)
    :down (do-move :down)
    :reset (reset)
    nil ()))

(goog.object/set js/window "onload"
      (fn []
        (anim/init)
        (let [read-state (get-state)]
          (if (nil? read-state)
            (game-logic/reset-state game-logic/game-state)
            (reset! game-logic/game-state (reader/read-string read-state))))
        (swap! game-logic/game-state (fn [old-state]
                                       (assoc old-state :state :playing)))
        (update-score game-logic/game-state)
        (animate-turn game-logic/empty-board (:board @game-logic/game-state) [])
        (goog.object/set js/window "onkeydown"
              (fn [e]
                (when (is-command (.-keyCode e))
                  (.preventDefault e)
                  (process-command (keycode-to-command (.-keyCode e))))))
        (goog.object/set (.getElementById js/document "reset-button") "onclick" reset)
        (goog.object/set (.getElementById js/document "keep-going-button") "onclick" keep-going)
        (goog.object/set (.getElementById js/document "new-game-button-2") "onclick" reset)
        (goog.object/set (.getElementById js/document "new-game-button") "onclick" reset)
        (swipe/init-listeners "board" do-move)))

