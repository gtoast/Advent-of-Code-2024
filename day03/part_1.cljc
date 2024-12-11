(ns aoc24.day03.part-1
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> (slurp (io/resource "aoc24/day03.txt"))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc24/day03.txt")))))

(def reg #"mul\((\d+),(\d+)\)")

(defn process [i]
   (let [matches (re-seq reg i)]
    (->> matches
         (map #(drop 1 %))
         (map #(map parse-long %))
         (map #(apply * %))
         (apply +))))

(defn -main
  "Run with (n)bb -m aoc24.day03.part-1"
  [& args]
  (->> input 
       process
       prn))

(comment
  
  (-main)

  (def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

  (def reg #"mul\((\d+),(\d+)\)")

  (let [matches (re-seq reg test-input)]
     (->> matches 
          (map #(drop 1 %))
          (map #(map parse-long %))
          (map #(apply * %))
          (apply +)))

  
  )