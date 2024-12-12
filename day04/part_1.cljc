(ns aoc24.day04.part-1
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (slurp (io/resource "aoc24/day04.txt")))
   :cljs
   (def input (await (slurp "resources/aoc24/day04.txt"))))

(defn format-input [i]
  (->> i
       (str/split-lines)
       (mapv #(vec %))))

 (defn xmas-match? [v]
  (or (= v [\X \M \A \S])
      (= v [\S \A \M \X])))

(defn forward-up-step [ws [r c]]
  (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (forward-up-step ws [(dec r) (inc c)]))))

(defn get-forward-up [world-scramble letter-coord]
  (take 4 (forward-up-step world-scramble letter-coord)))
 
(defn forward-step [ws [r c]]
  (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c])
                  (forward-step ws [r (inc c)]))))

(defn get-forward [word-scramble letter-coord] 
  (take 4 (forward-step word-scramble letter-coord)))


(defn forward-down-step [ws [r c]]
        (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (forward-down-step ws [(inc r) (inc c)]))))
      
(defn get-forward-down [world-scramble letter-coord]
  (take 4 (forward-down-step world-scramble letter-coord)))

 (defn down-step [ws [r c]]
   (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (down-step ws [(inc r) c]))))

(defn get-down [word-scramble letter-coord]
  (take 4 (down-step word-scramble letter-coord)))

(defn check-all [word-scramble letter-coord]
  ((juxt get-forward-up get-forward get-forward-down get-down) word-scramble letter-coord))

(defn process [i]
  (->> (for [row (range (count i)) col (range (count (first i)))]
         (->> (check-all i [row col])
              (filter (comp xmas-match? (fn [four-letter-and-coords]
                                          (map first four-letter-and-coords))))))
       (remove empty?)
       (apply concat)
       count))

(defn solve [i]
  (->> i
       format-input
       process))

(defn -main
  "Run with (n)bb -m aoc24.day04.part-1"
  [& args]
  (->> input 
       solve
       prn))


(comment

  (def test-input
    "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

  (def dot-or-xmas
    "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX")

  (->> test-input
       format-input
       process)


  (->> test-input
       (str/split-lines)
       (mapv #(vec %)))

  (def formatted-test-input (->> test-input
                                 format-input))

  (get-in formatted-test-input [0 -3])

  (= #{[0 5] [0 8]}
     #{[0 8] [0 5]})



  ;check forward
  (->> [formatted-test-input [0 5]]
       (apply
        (fn [input [row col]]
          (for [c (range col (+ 4 col))]
            ((juxt (partial get-in input) identity) [row c])))))

  (->> [formatted-test-input [0 5]]
       (apply
        (fn [input [row col]]
          (for [c (range col (- 4 col))]
            ((juxt (partial get-in input) identity) [row c])))))

  (->> [0 5]
       #(take 4 (forward-steps %)))

  ;check forward
  (->> [formatted-test-input [0 5]]
       (apply
        (fn forward-step [ws [r c]]
          (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (forward-step ws [r (inc c)])))))
       (take 4))

  ;check foward-down
  (->> [formatted-test-input [0 4]]
       (apply
        (fn forward-down-step [ws [r c]]
          (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (forward-down-step ws [(inc r) (inc c)])))))
       (take 4))

  ;check down
  (->> [formatted-test-input [1 6]]
       (apply
        (fn down-step [ws [r c]]
          (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (down-step ws [(inc r) c])))))
       (take 4))

  ;check forward-up
  (->> [formatted-test-input [5 0]]
       (apply
        (fn forward-up-steps [ws [r c]]
          (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (forward-up-steps ws [(dec r) (inc c)])))))
       (take 4))

  ;out of bounds steps are full of nils
  (->> [formatted-test-input [0 0]]
       (apply
        (fn forward-up-step [ws [r c]]
          (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (forward-up-step ws [(dec r) (inc c)])))))
       (take 4))

;backwards steps but do we need it?
  (->> [formatted-test-input [0 5]]
       (apply
        (fn backward-steps [ws [r c]]
          (lazy-seq (cons ((juxt (partial get-in ws) identity) [r c]) (backward-steps ws [r (dec c)])))))
       (take 4))


  (get-forward-up formatted-test-input [5 0])
  (get-forward-down formatted-test-input [0 4])
  (get-forward formatted-test-input [0 5])
  (get-down formatted-test-input [1 6])


  (defn check-all [word-scramble letter-coord]
    ((juxt get-forward-up get-forward get-forward-down get-down) word-scramble letter-coord))

  (def processed-test-input
    (->> (for [row (range (count formatted-test-input)) col (range (count (first formatted-test-input)))]
           (->> (check-all formatted-test-input [row col])
                (filter (comp xmas-match? (fn [four-letter-and-coords]
                                            (map first four-letter-and-coords))))))
         (remove empty?)
         (apply concat)))

  (count processed-test-input)

  (def processed-test-input-map (->> processed-test-input
                                     (mapcat #(map (fn [[v k]] (hash-map k v)) %))
                                     (apply merge)))

  (def processed-test-input-map-and-dots
    (for [row (range (count formatted-test-input))]
      (for [col (range (count (first formatted-test-input)))]
        (processed-test-input-map [row col] \.))))

  )



