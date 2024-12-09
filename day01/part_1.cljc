(ns aoc24.day01.part-1 
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input  (slurp (io/resource "aoc24/day01.txt")))
   :cljs
   (def input (await (slurp "resources/aoc24/day01.txt"))))

(defn format-input [s]
  (let [data (->> s
                   (str/split-lines)
                   (map #(str/split % #"\s+"))
                   (map #(map (fn [n] (Integer/parseInt n)) %)))]
    data))

(defn process [d]
  (let [c (count d)]
    (->> (apply interleave d)
         (split-at c)
         (map #(sort %));sorted from smallest
         (apply #(map - %1 %2))
         (transduce (map abs) +))))

(defn -main
  "Run with (n)bb -x aoc24.day01.part-2"
  [& _args]
  (->> input
       format-input
       process
       prn))



(comment 
  
(let [s input
      result (->> s
                 (str/split-lines)
                 (map #(str/split % #"\s+"))
                 (map #(map (fn [n] (Integer/parseInt n)) %)))]
  result)

  (def test-input
    "3   4
4   3
2   5
1   3
3   9
3   3")
  
  (format-input input)
  (part-1)
  
  (defn format-input [s]
    (let [input (->> s
                     (str/split-lines)
                     (map #(str/split % #"\s+"))
                     (map #(map (fn [n] (Integer/parseInt n)) %)))]
      input)) 
  
 (defn process [d]
    (let [c (count d)]
      (->> (apply interleave d)
           (split-at c)
           (map #(sort %));sorted from smallest
           (apply #(map - %1 %2))
           (transduce (map abs) +))))
 
  (process)
  
  #_(defn process [d]
    (->> d
         (map #(apply - %))
         flatten
         (map abs)))

  (defn process [d]
    (let [c (count d)]
      (->> (apply interleave d)
           (split-at c)
           (map #(sort %));sorted from smallest
           (apply #(map - %1 %2))
           (transduce (map abs) +)
           )))
           
  (count data)
  
  (interleave '(3 4) '(4 3) '(2 5) '(1 3) '(3 9) '(3 3)) 
  (apply #(map - %1 %2) (process data))
             ;;=> ((1 2 3 3 3 4) (3 3 3 4 5 9))
  )