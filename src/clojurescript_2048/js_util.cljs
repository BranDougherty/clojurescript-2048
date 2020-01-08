(ns clojurescript-2048.js-util)

(defn set-style
  [ele property value]
  (goog.object/set (goog.object/get ele "style") property value))

(defn remove-all-children
  [ele]
  (loop []
    (when (.hasChildNodes ele)
      (.removeChild ele (.-lastChild ele))
      (recur))))

