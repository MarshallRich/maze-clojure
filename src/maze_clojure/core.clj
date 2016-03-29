(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []
  (vec
    (for [row (range size)]
      (vec
        (for [col (range size)]
          {:row row :col col :visited? false :bottom? true :right? true})))))

(defn possible-neighbors [rooms row col]
  (vec  
   (remove 
     (fn [room]
       (or (nil? room) (:visited? room)))
    [(get-in rooms [(dec row) col])
     (get-in rooms [(inc row) col])
     (get-in rooms [row (dec col)])
     (get-in rooms [row (inc col)])])))

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))
        
(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ;going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    ;going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ;going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ;going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))

(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visited?] true)
        next-room (random-neighbor rooms row col)]
    (if next-room
      (let [rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (loop [old-rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
          (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room))]
            (if (= old-rooms new-rooms)
              old-rooms
              (recur new-rooms)))))
      rooms)))

(defn -main []
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)]
    ;print top walls
    (doseq [_ rooms]
      (print " _"))
    (println)
    
    ;print grid
    (doseq [row rooms]
      (print "|")
      (doseq [room row]
        (if (:bottom? room)
          (print "_")
          (print " "))
        (if (:right? room)
          (print "|")
          (print " ")))
     (println))))
