(ns aoc24.day04.part-2
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

(defn match-mas?
  "Does the sequence match SAM or MAS?"
  [v]
  (or (= v '(\M \A \S))
      (= v '(\S \A \M))))

 (defn match-x-mas? 
   "Check the two sequences for a X-MAS"
   [[f s]] 
   (and (match-mas? (map first f))
        (match-mas? (map first s)))) 

;get forward-up
(defn get-forward-up 
  "Get the diagonal up starting at row, col"
  [word-scramble row col]
  (->> [word-scramble row col]
       (apply
        (fn forward-up-steps [ws r c]
          (lazy-seq (cons (->> ((juxt (partial get-in ws) identity) [r c]) (apply cons)) (forward-up-steps ws (dec r) (inc c))))))
       (take 3)))

(defn get-forward-down 
  "Get the diagonal down starting at row, col"
  [word-scramble row col]
  (->> [word-scramble row col]
     (apply
      (fn forward-down-step [ws r c]
        (lazy-seq (cons (->> ((juxt (partial get-in ws) identity) [r c]) (apply cons)) (forward-down-step ws (inc r) (inc c))))))
     (take 3)))

  (defn get-forward-down-x 
    "Get the X below starting and row, col and row + 2, col"
    [ws r c] 
    (->> [ws r c]
       (apply
        (juxt get-forward-down #(get-forward-up %1 (+ %2 2) %3))))) 

(defn process [i]
  (->> (for [row (range (count i))
             col (range (count (first i)))]
         (get-forward-down-x i row col))
       (filter match-x-mas?)
       count))

(defn solve [i]
  (->> i
       format-input
       process))


(defn -main
  "Run with (n)bb -m aoc24.day04.part-2"
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
  
  (def test-input-or-dots
    ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........")
  
  (def formatted-test-input
    (->> test-input format-input))
  
 ;check forward-up
  (->> [formatted-test-input 4 1]
       (apply
        (fn forward-up-steps [ws r c]
          (lazy-seq (cons (->> ((juxt (partial get-in ws) identity) [r c]) (apply cons)) (forward-up-steps ws (dec r) (inc c))))))
       (take 3))

  ;check foward-down
  (->> [formatted-test-input 2 1]
       (apply
        (fn forward-down-step [ws r c]
          (lazy-seq (cons (->> ((juxt (partial get-in ws) identity) [r c]) (apply cons)) (forward-down-step ws (inc r) (inc c))))))
       (take 3))
  
  

  ;forward up x
  (def m-s
    ["M.S" 
     ".A."
     "M.S"])

  (def s-m 
    ["S.M"
     ".A."
     "S.M"])
  
  (def s-s  
    ["S.S"
     ".A."
     "M.M"])
  
  (def m-m
    ["M.M"
     ".A."
     "S.S"])
  
  (def bad-x
    (->> 
     ["S.M"
      ".A."
      "M.M"]))
  
  ;get forward-up-x
  (->> [s-m 2 0]
       (apply
        (juxt get-forward-up #(get-forward-down %1 (- %2 2) %3)))) 
  
  
  ;get forward-down-x
  (def forward-down-x 
    (->> [m-m 0 0]
         (apply
          (juxt get-forward-down #(get-forward-up %1 (+ %2 2) %3)))))
  

  
  ;bad forward-down-x
  (def bad-forward-down-x
    (->> [bad-x 0 0]
         (apply
          (juxt get-forward-down #(get-forward-up %1 (+ %2 2) %3))))) 
  
  (let [[f s] forward-down-x]
    (and (match-mas? (map first f))
         (match-mas? (map first s))))
  
  (let [[f s] bad-forward-down-x]
    (and (match-mas? (map first f))
         (match-mas? (map first s))))
  
  (->> formatted-test-input 
       ((fn process [word-scramble]
           (for [row (range (count word-scramble)) 
                 col (range (count (first word-scramble)))]
             (->> [word-scramble row col]
                  (apply get-forward-down-x)))))
       (filter match-x-mas?) 
       count)
  
  (->> test-input
       format-input
       process)
  )