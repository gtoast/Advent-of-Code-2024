(ns aoc24.day02.part-2
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> (slurp (io/resource "aoc24/day02.txt"))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc24/day02.txt")))))

(defn format-input [i]
    (->> i
         (str/split-lines)
         (map #(str/split % #"\s+"))
         (map #(map parse-long %))))

(defn safe? [lvls]
  (let [pairs (partition 2 1 lvls)]
    (and (every? (fn [[n m]] (<= (abs (- n m)) 3)) pairs)
         (or (every? #(apply > %) pairs) (every? #(apply < %) pairs)))))

(defn drops [lvls]
  (let [ns (range (count lvls))]
    (for [n ns]
      (->> lvls (split-at n) ((fn [[f s]] [f (drop 1 s)])) (apply concat)))))


(defn process [i]
  (->> (for [l i]
         (some? (some safe? (cons l (drops l))))) 
       (filter true?)
       count))




(defn -main
  "Run with (n)bb -m aoc24.day02.part-2"
  [& _]
  (->> input 
       format-input
       process
       prn))

(comment
  (def test-input
    "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

  (defn format-input [i]
    (->> i
         (str/split-lines)
         (map #(str/split % #"\s+"))
         (map #(map parse-long %))))

  (def formatted-input (format-input test-input))
  (def f (first formatted-input))
  (def s (second formatted-input))
  
  (defn drops [lvls]
    (let [ns (range (count lvls))]
      (for [n ns] 
        (->> lvls (split-at n) ((fn [[f s]] [f (drop 1 s)])) (apply concat)))))
  (range (count f))
  
  (map safe? (drops (nth formatted-input 3)))
  (some safe? (drops (nth formatted-input 3)))

  (->> f (split-at 0) ((fn [[f s]] [f (drop 1 s)])) (apply concat))

  (->> f
       (partition 2 1)
       (every? #(apply > %)))
  (->> s
       (partition 2 1)
       (every? #(apply < %)))

  (->> (nth formatted-input 4)
       (partition 2 1)
       (every? #(apply > %)))

  (->> (nth formatted-input 0)
       (partition 2 1)
       (every? (fn [[n m]] (<= (abs (- n m)) 3))))

  (defn safe? [lvls]
    (let [pairs (partition 2 1 lvls)]
      (and (every? (fn [[n m]] (<= (abs (- n m)) 3)) pairs)
           (or (every? #(apply > %) pairs) (every? #(apply > %) pairs)))))

  (map safe? formatted-input)
  (-main)
  )
