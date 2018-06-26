(ns tic.core
  (:require[clojure.pprint :refer [pprint]]
           [clojure.inspector :refer [inspect-table]]
           )
  (:gen-class))

(def grid-full (atom [
                 ["X" " " "X"]
                 [" " "X" "X"]
                 [" " " " "X"]
                 ]))

(def grid (atom [
                 [" " " " " "]
                 [" " " " " "]
                 [" " " " " "]
                 ]))


(defn update! [pos char]
  (let [r (quot pos 3)
        c (mod pos 3)]
    (println "updating " r c )
    (swap! grid assoc-in [r c] char))
  )

(defn diag-1 [grid]
  (map #(nth (nth grid  %) % ) '[0 1 2])
  )

(defn diag-2 [grid]
  (map #(nth (nth grid (- 2 %)) % ) '[0 1 2])
  )


(defn winner-row? [grid char r]
  (let [row (nth grid r)]
    (apply = char row)
    )
  )

(defn winner-col? [grid char c]
  (let [col (map #(nth % c) grid)]
    (apply = char col)))

(defn winner-by-rows? [grid char]
  (or (winner-row? grid char 0)
      (winner-row? grid char 1)
      (winner-row? grid char 2)))

(defn winner-by-cols? [grid char]
  (or (winner-col? grid char 0)
      (winner-col? grid char 1)
      (winner-col? grid char 2)))


(defn winner-by-diags? [grid char]
  (let [d1 (diag-1 grid)
        d2 (diag-2 grid)]
    (or  (apply = char d1)
         (apply = char d2))))

(winner-by-rows? @grid "X")

(winner-by-cols? @grid "X")

(winner-by-diags? @grid "X")

(winner-by-diags? @grid "O")

(defn winner? [grid char]
  (or
   (winner-by-rows? grid char)
   (winner-by-cols? grid char)
   (winner-by-diags? grid char)))

(diag-2 @grid)
(diag-1 @grid)

(winner? @grid "O")

;;(clojure.inspector/inspect-table  @grid-full)


(defn window [grid]
  (let [r (clojure.inspector/inspect  @grid)]
    (println (type r))
    (Thread/sleep 1000)
    (.setVisible r false)
    (.dispose r)
    (println "after"))
  )

(defn print-grid [grid]
  (println (map #(str % "\n") grid)))

(defn other-player [c]
  (if (= "X" c) "O"
      "X"))

(defn draw? [grid]
  (and (not (winner? grid "O")) (not (winner? grid "X"))
       (every? #(not= " " %) (flatten grid)))
  )

(defn legal? [grid pos]
  (let [r (quot pos 3)
        c (mod pos 3)]
    (= " " (get-in grid [r c]))))

;; OVERall loop structure poor
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, player!")

  (print-grid @grid)
  (loop
      [ player "X"
       _  (println "choose a position 1-9 for" player)
    line  (read-line)
   ]
    (when (re-matches #"[0-9]" line)
      (let [pos (dec (read-string line))
            legal (legal? @grid pos)]
        (if legal
          (update! pos player)
          (println "not valid play"))));; but swap player
    (print-grid @grid)
    (if (winner? @grid player) (println "Player" player "wins"))
    (if (draw? @grid) (println "Another draw!"))
    (if (or (draw? @grid) (winner? @grid player) (= line "q"))
      (println "Bye!")
      (do
        (println "choose a position 1-9 for" (other-player player))
        (recur  (other-player player) nil (read-line))))))
