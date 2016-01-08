(ns flood-it.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent]
            [re-frame.core :refer [register-handler
                                   path
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]))

(enable-console-print!)

(defn random-board [size colors]
  (let [row (fn [] (vec (repeatedly size #(rand-int colors))))]
    (vec (repeatedly size row))))

(defn storage-get-int [key]
  (js/parseInt (or (.getItem (.-localStorage js/window) key) "0")))

(defn storage-set-int [key val]
  (.setItem (.-localStorage js/window) key val))

(defn initial-state [size colors]
  {:size size
   :colors colors
   :moves 0
   :board (random-board size colors)
   :wins (storage-get-int "wins")
   :games (storage-get-int "games")})

(defn win-state [db]
  (cond
    (every? #(= (get-in db [:board 0]) %) (:board db)) :win
    (>= (:moves db) 25) :loss
    :else nil))

;; -- Event Handlers ----------------------------------------------------------


(register-handler                 ;; setup initial state
 :initialize                     ;; usage:  (submit [:initialize])
 (fn
   [db _]
   (merge db (initial-state 14 6))))    ;; what it returns becomes the new state


(register-handler
 :new-game
 (fn [db _]
    (assoc db :board (random-board (:size db) (:colors db))
              :moves 0)))


(defn get-neighbors [[x y]]
  [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]])

(defn find-cells [board size]
  (let [color (get-in board [0 0])]
    (loop [seen #{}
           q [[0 0]]]
        (cond
          (empty? q) (vec seen)
          (contains? seen (peek q)) (recur seen (pop q))
          (not= (get-in board (peek q)) color) (recur seen (pop q))
          ((fn [[x y]] (or (< x 0) (< y 0) (>= x size) (>= y size))) (peek q))
          (recur seen (pop q))
          :else (recur (conj seen (peek q)) (vec (concat (pop q) (get-neighbors (peek q)))))))))

(defn assoc-in-mult [coll paths val]
  (loop [p paths
         c coll]
    (if (empty? p)
      c
      (recur (pop p) (assoc-in c (peek p) val)))))

(register-handler
 :flood
 (fn
   [db [_ color]]
   (if (or (win-state db) (= color (get-in db [:board 0 0])))
    db
    (let [board (:board db)
          db (assoc db
              :board (assoc-in-mult board (find-cells board (:size db)) color)
              :moves (inc (:moves db)))]
      (if-not (win-state db) db
        (let [res (win-state db)
              wins (+ (:wins db) (if (= res :win) 1 0))
              games (inc (:games db))]
           (do
             (storage-set-int "wins" wins)
             (storage-set-int "games" games)
             (assoc db :wins wins :games games))))))))



;; -- Subscription Handlers ---------------------------------------------------


(register-sub
 :cell
 (fn
   [db [x y]]
   (reaction (get-in (:board @db) [x y]))))

(defn register-key-sub [key]
  (register-sub
    key
    (fn [db _] (reaction (get @db key)))))

(register-key-sub :moves)
(register-key-sub :size)
(register-key-sub :board)
(register-key-sub :colors)
(register-key-sub :wins)
(register-key-sub :games)

;; -- View Components ---------------------------------------------------------

(def color-map ["#f3f61d","#7e8d1e","#605ca8", "#46b1e2", "#ed70c1", "#dc4a20"])


(defn cell
 [color]
 [:span.cell {:style {:background-color (get color-map color)}}])

(defn grid [board size]
    [:div.grid {:style {:width (* size 20)}}
        (for [x (range size)]
            ^{:key x} [:div.row
                        (for [y (range size)]
                          ^{:key y} [cell (get-in board [x y])])])])

(defn controls
  [colors finished]
  [:div.controls {:style {:width (* colors 35)}}
      (for [i (range colors)]
         ^{:key i} [:span.control {:style {:background-color (get color-map i)
                                           :cursor (if-not finished "pointer" "initial")}
                                   :on-click #(dispatch [:flood i])}])])

(defn new-game [] [:button {:on-click #(dispatch [:new-game])} "New Game"])

(defn game-end-msg [board moves]
  (let [win (win-state {:board board :moves moves})]
    (case win
      :win [:div [:h2.win-msg "You win!"] [:br] [new-game]]
      :loss [:div [:h2.loss-msg "You lose."] [:br] [new-game]]
      [:div])))

(defn game []
  (let [board (subscribe [:board])
        size  (subscribe [:size])
        colors (subscribe [:colors])
        moves (subscribe [:moves])
        wins (subscribe [:wins])
        games (subscribe [:games])]
     (fn [] [:div.game
              [:h1 "Flood It!"]
              [:div.col-left [game-end-msg @board @moves]]
              [:div.col-middle
                [grid @board @size]
                [:br] [controls @colors (win-state {:board @board :moves @moves})]
                [:p {:style {:text-align "left"}} "Flood the board until everything is the same color.
                     Your blob starts in the top left corner of the board, and you can grow
                     by matching the color of the surrounding squares.
                     Click one of the boxes in the color palette to get started.
                     Try to make it in uder 25 moves!"]
                [:p [:small "This is not my original game idea. It is a blatant
                  rip-off of the game Flood-It! by Lab Pixes available on "]
                  [:a {:href "https://play.google.com/store/apps/details?id=com.labpixies.flood&hl=en"}
                      [:small "Google Play"]]]]
              [:div.col-right [:h2 "Moves: " @moves "/25"] [:br]
                              [:h3 "Win Rate: " @wins "/" @games]]])))


;; -- Entry Point -------------------------------------------------------------



(dispatch-sync [:initialize])
(reagent/render [game]
                (js/document.getElementById "app"))
