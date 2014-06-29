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

(def width 5)
(def height 5)
(def n-pieces (/ (* width height) 2))


(def indices  (for [r (range 0 width) p (range 0 height)] [r p]))

(defn generate-board []
  (let [ get-by-index (fn [v [x y]] (get v (+ x (* y width))))
         pieces (range 0 n-pieces)
         numbers (shuffle  (apply conj pieces pieces))
         board (map (fn [index] {index (get-by-index numbers index)}) indices)]
    (.log js/console (.toString board))
    (reduce merge board)))

(def app-state (atom {}))

(defn sort-row [row]
  (sort (fn [[[_ y] _] [[_ y2] _]] (< y y2)) row))

(defn get-row [board nth]
  (sort-row (filter (fn [[[x _] _]] (= x nth)) board)))

(defn conj-discovered [cursor & args]
  (update-in cursor [:discovered] #(conj % args)))

(defn handle-click [e cursor cur-index]
  (let [ch (chan)]
    (om/update! cursor
                (fn [{:keys [board clicked paused] :as cursor}]
                  (if paused
                    cursor
                    (let [cur-piece (get board cur-index)
                          last-piece (:piece clicked)
                          last-index (:index clicked)
                          first-click? (nil? clicked)
                          new-found? (and (= last-piece cur-piece)
                                          (not (= last-index cur-index)))]
                      (cond
                        first-click? (-> cursor
                                         (assoc :clicked {:piece cur-piece :index cur-index})
                                         (update-in [:discovered] #(conj % cur-index)))
                        new-found? (do
                                     (.log js/console "new found!")
                                     (-> cursor
                                         (dissoc :clicked)
                                         (update-in [:found] #(conj % cur-index last-index))
                                         (update-in [:discovered] #(conj % cur-index last-index))))
                        :else (do
                                (.log js/console "fail" (:discovered cursor))
                                (go (>! ch [cur-index (:index clicked)]))
                                (-> cursor
                                    (dissoc :clicked)
                                    (assoc :paused true)
                                    (update-in [:discovered] #(conj % cur-index last-index)))))))))
    (go (let [[a b] (<! ch)]
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
      (om/update! cursor #(assoc % :board (generate-board) :clicked nil :found #{} :discovered #{} )))
    om/IRender
    (render [this]
      (let [{:keys [board found clicked discovered]} cursor]
        (apply dom/div #js {:className "o"}

               (for [y (range 0 height)
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
                                  (str "(" v ")"))))))))))


(defn tutorial-app [cursor owner]
  (.log js/console "owner" owner)
  (reify
    om/IRender
    (render [this]
      (dom/div nil
               (om/build board-matrix cursor {})))))

(om/root app-state tutorial-app (.getElementById js/document "content"))