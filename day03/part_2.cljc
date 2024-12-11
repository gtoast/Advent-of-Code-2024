(ns aoc24.day03.part-2
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (->> (slurp (io/resource "aoc24/day03.txt"))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc24/day03.txt")))))

(def reg #"mul\((\d+),(\d+)\)|(don\'t)\(\)|(do)\(\)")

(defn format-input [i]
  (let [matches (re-seq reg i)]
    (->> matches
         (map #(remove nil? %))
         (map #(drop 1 %)))))

(defn handle-conditionals [i]
  ;start off by taking however many ("n" "m") pairs off the top
  ; up until the next conditional "do" or "don't".
  (loop  [instr  (drop-while #(= 2 (count %)) i)  
          result (take-while #(= 2 (count %)) i)]
    (if (empty? instr)  ;if instruction list is empty
      result ;we're done
      (condp = (first instr) ;else get the conditional and drop/take as needed
        '("do")  (recur (drop-while #(= 2 (count %)) (rest instr)) (concat result (take-while #(= 2 (count %)) (rest instr))))
        '("don't") (recur (drop-while #(= 2 (count %)) (rest instr)) result)))))

(defn process [i]
  (->> i
       handle-conditionals
       (map #(map parse-long %))
       (map #(apply * %))
       (apply +)))

(defn solve [i]
  (->> i
       format-input
       process))

(defn -main
  "Run with (n)bb -m aoc24.day03.part-1"
  [& args] 
  (->> input
       solve
       prn))

(comment

  (solve test-input)
  (solve input)


  (defn process [i]
    (let [matches (re-seq reg i)]
      (->> matches
           (map #(remove nil? %))
           (map #(drop 1 %))
           handle-conditionals
           (map #(map parse-long %))
           (map #(apply * %))
           (apply +))))

  (process test-input)


  (defn handle-conditionals [i]
    (loop  [instr  (drop-while #(= 2 (count %)) i)
            result (take-while #(= 2 (count %)) i)]
      instr ; (("don't") ("5" "5") ("11" "8") ("do") ("8" "5"))
      result ; (("2" "4"))
      (if (empty? instr)
        result
        (condp = (first instr)
          '("do")  (recur (drop-while #(= 2 (count %)) (rest instr)) (concat result (take-while #(= 2 (count %)) (rest instr))))
          '("don't") (recur (drop-while #(= 2 (count %)) (rest instr)) result)))))


  (cond ("don't")
        ("don't") "hello"
        "what?")



  (count (first (process test-input)))

  (take-while #(> (count %) 1)  (process test-input))





  #_(do (vreset! *do* (first (first instr)))
        (if (= *do* "do")
          (loop (drop-while #(> 1 (count %)) instr))))


  (def test-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


  (let [matches (re-seq reg test-input)]
    (->> matches
         (map #(remove nil? %))
         (map #(drop 1 %))
         (map #(map parse-long %))
         (map #(apply * %))
         (apply +)))
  )
  