(ns memory.app
  (:require-macros [cljs.core.async.macros :refer [go alt!]]
                   [secretary.macros :refer [defroute]])
  (:require [goog.events :as events]
            [cljs.core.async :refer [put! <! >! chan timeout]]
            [markdown.core :as md]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [secretary.core :as secretary]
            [cljs-http.client :as http])
  (:import [goog History]
           [goog.history EventType]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components

(def INITIAL [])

(def width 6)
(def height 6)
(def n-pieces (/ (* width height) 2))



(def indices  (for [r (range 0 width) 
                    p (range 0 height)] [r p]))

(defn generate-board []
  (let [ get-by-index (fn [v [x y]] (get v (+ x (* y width))))
         pieces (range 0 n-pieces)
         numbers (shuffle  (apply conj pieces pieces))
         board (map (fn [index] {index (get-by-index numbers index)}) indices)]
    (.log js/console (.toString board))
    (reduce merge board)))

(def app-state (atom {}))

(defn handle-click [e cursor cur-index]
  (let [pauseCh (chan)]
    (om/update! cursor
                (fn [{:keys [board clicked paused] :as cursor}]
                  (if paused
                    cursor
                    (let [cur-piece (get board cur-index)
                          prev-piece (:piece clicked)
                          prev-index (:index clicked)
                          first-discovered? (nil? clicked)
                          new-pair-found? (and (= prev-piece cur-piece)
                                               (not (= prev-index cur-index)))]
                      (cond
                        first-discovered? (-> cursor
                                            (assoc :clicked {:piece cur-piece :index cur-index})
                                            (update-in [:discovered] #(conj % cur-index)))
                        new-pair-found? (do
                                          (.log js/console "new found!")
                                          (.log js/console (toString (:discovered cursor)))
                                          (-> cursor
                                            (dissoc :clicked)
                                            (update-in [:found] #(conj % cur-index prev-index))
                                            (update-in [:discovered] #(conj % cur-index prev-index))
                                            (update-in [:n-left-to-discover] dec)))
                        :else (do
                                (.log js/console "fail" (:discovered cursor))
                                (go (>! pauseCh [cur-index (:index clicked)]))
                                (-> cursor
                                    (dissoc :clicked)
                                    (assoc :paused true)
                                    (update-in [:discovered] #(conj % cur-index prev-index)))))))))
    (go (let [[a b] (<! pauseCh)]
          (<! (timeout 1000))
          (om/update! cursor
                      (fn [c]
                        (-> c
                            (dissoc :paused)
                            (update-in [:discovered] #(disj % a b)))))))))


(defn board-matrix
  [cursor owner opts]
  (reify
    om/IInitState
    (init-state [this]
      (om/update! cursor #(assoc % :board (generate-board) 
                                   :clicked nil 
                                   :found #{} 
                                   :discovered #{} 
                                   :n-left-to-discover (/ (* width height) 2)
                                   )))
    om/IRender
    (render [this]
      (let [{:keys [board found clicked discovered n-left-to-discover]} cursor
          board (for [y (range 0 height)
                      x (range -1 width)]
                  (let [v (get board [x y])
                        clicked? (= (:index clicked) [x y])
                        found? (contains? found [x y])
                        discovered? (contains? discovered [x y])
                        className (str
                                    (when clicked? " clicked")
                                    (when found? " found")
                                    (when discovered? " discovered")
                                    " r")
                        clickable? (and (not clicked?)
                                        (not found?))]
                        (if (== -1 x)
                          (dom/div #js {:className (str "c1 r")} "")
                          (dom/div #js {:className className :onClick (when clickable? #(handle-click % cursor [x y]))}
                            (str "(" v ")")))))
          left-to-discover (dom/div #js {:className "n-left-to-discover"} (str "Left to discover " n-left-to-discover))
          victory (dom/div nil (if (= n-left-to-discover 0) "Victory!" "")) ]
        (dom/div nil
          (apply dom/div #js {:className "o"}    
            board)
          left-to-discover
          victory)))))

(defn tutorial-app [cursor owner]
  (.log js/console "owner" owner)
  (reify
    om/IRender
    (render [this]
      (dom/div nil
               (om/build board-matrix cursor {})))))

(om/root app-state tutorial-app (.getElementById js/document "content"))