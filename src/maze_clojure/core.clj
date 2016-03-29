(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []
  (vec
    (for [row (range size)]
      (vec
        (for [col (range size)]
          {:row row :col col :visited? false :bottom? true :right? true})))))

(defn -main []
  (let [rooms (create-rooms)]
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
