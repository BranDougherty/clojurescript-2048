(ns clojurescript-2048.game-logic
  (:require [clojure.set :as set]))

(def empty-board (into [] (repeat 4 (into [] (repeat 4 nil)))))

(defn column
  [board n]
  (map #(nth %1 n) board))

(defn transpose
  [board]
  (map #(column board %1) (range (count board))))

(defn pad
  [coll val n]
  (loop [vec-coll (into [] coll)]
    (if (> n (count vec-coll))
      (recur (conj vec-coll val))
      vec-coll)))

(defn fall
  [coll]
  (filter #(not (nil? %1)) coll))

(defn do-collapse
  [coll acc merges]
  (if (empty? coll)
    [acc merges]
    (let [bottom (first coll)
          next-item (second coll)]
      (if-not (= (:value bottom) (:value next-item))
        (do-collapse (rest coll) (conj acc bottom) merges)
        (do-collapse (drop 2 coll)
                     (conj acc (assoc next-item :value (* (:value next-item) 2)))
                     (conj merges {:destroyed bottom :into next-item}))))))

(defn collapse
  [coll]
  (do-collapse coll [] []))

(defn shift
  [coll]
  (let [[new-coll merges] (collapse (fall coll))]
    [(pad new-coll nil (count coll)) merges]))

(defn shift-right
  [coll]
  (let [[new-coll merges] (shift (reverse coll))]
    [(reverse new-coll) merges]))

(defn shift-board-in-dir
  [board shift-fn]
  (reduce (fn [[rows merges] [new-row new-merges]]
            [(conj rows new-row)
             (into merges new-merges)])
          [[] []]
          (map shift-fn board)))

(defn shift-board-left
  [board]
  (shift-board-in-dir board shift))

(defn shift-board-right
  [board]
  (shift-board-in-dir board shift-right))

(defn shift-board-up
  [board]
  (let [[new-board merges] (shift-board-left (transpose board))]
    [(transpose new-board) merges]))

(defn shift-board-down
  [board]
  (let [[new-board merges] (shift-board-right (transpose board))]
    [(transpose new-board) merges]))

(defn shift-board
  [board direction]
  ((case direction
     :left shift-board-left
     :right shift-board-right
     :up shift-board-up
     :down shift-board-down) board))

(def initial-state {
                    :board [[nil nil nil nil]
                            [nil nil nil nil]
                            [nil nil nil nil]
                            [nil nil nil nil]]
                    :score 0
                    :high-score 0
                    :state :playing
                    :waiting-for-win true
                    })

(def game-state (atom initial-state))

(defn indexed-board
  [board]
  (reduce into [] (map-indexed (fn [row-index row]
                                 (map-indexed (fn [cell-index cell]
                                                [row-index cell-index cell]) row)) board)))

(defn get-nil-indices
  [board]
  (filter #(nil? (%1 2)) (indexed-board board)))

(defn id-mapped-board
  [board]
  (reduce into {} (map (fn [[row-index cell-index cell]]
                         { (:id cell) [
                                       row-index
                                       cell-index
                                       cell
                                       ]}) (indexed-board board))))

(defn new-id
  [old-board new-board]
  (let [get-ids (fn [board]
                  (map :id (filter #(not (nil? %1)) (reduce into [] board))))
        all-ids (set/union (set (get-ids old-board)) (set (get-ids new-board)))]
    (reduce (fn [curr-id next-id]
              (if-not (contains? all-ids curr-id)
                (reduced curr-id)
                next-id))
            (iterate inc 0))))

(defn get-random-cell
  [old-board new-board]
  {:id (new-id old-board new-board) :value (rand-nth [2 2 2 4])})

(defn try-insert-random-cell
  [old-board new-board]
  (let [nil-indices (get-nil-indices new-board)]
    (if (empty? nil-indices)
      new-board
      (let [[row-index cell-index] (rand-nth nil-indices)
            vec-new-board (into [] (map #(into [] %1) new-board))]
        (assoc-in vec-new-board [row-index cell-index] (get-random-cell old-board new-board))))))

(defn move
  [board direction]
  (let [[new-board merges] (shift-board board direction)]
    (if-not (= board new-board)
      [(try-insert-random-cell board new-board) merges]
      [new-board merges])))

(defn new-board
  []
  (nth (iterate #(try-insert-random-cell empty-board %1) empty-board) 2))

(defn turn-score
  [merges]
  (apply + (map #(* (get-in %1 [:destroyed :value]) 2) merges)))

(defn values
  [board]
  (map #(:value %1 0) (reduce into [] board)))

(defn won?
  [board]
  (boolean (some #{2048} (values board))))

(defn lost?
  [board]
  (if-not (zero? (count (get-nil-indices board)))
    false
    (let [[left-board] (shift-board board :left)
          [right-board] (shift-board board :right)
          [up-board] (shift-board board :up)
          [down-board] (shift-board board :down)]
      (= (values left-board)
         (values right-board)
         (values up-board)
         (values down-board)
         (values board)))))

(defn reset-state
  [state]
  (let [high-score (:high-score @state)
        nb (new-board)]
    (reset! state initial-state)
    (swap! state #(assoc %1
                         :board nb
                         :high-score high-score))))

