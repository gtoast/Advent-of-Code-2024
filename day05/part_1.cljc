(ns aoc24.day05.part-1
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [combinations]]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (slurp (io/resource "aoc24/day05.txt")))
   :cljs
   (def input (await (slurp "resources/aoc24/day05.txt"))))

(defn format-input [input] 
    (->> input
         (str/split-lines)
         (split-with seq)
         ((fn [[f l]]
            (hash-map
             :rules
             (->> f
                  (map #(re-find #"(\d+)\|(\d+)" %))
                  (map #(drop 1 %)))
             :updates
             (->> (rest l)
                  (map #(str/split % #","))))))))

(defn find-index [coll element]
  (ffirst (filter #(= element (second %)) (map-indexed vector coll))))
  
(defn get-relevant-rules [rules pages]
  (let [pairs (map set (combinations pages 2)) 
        rule-set-to-rule (->> (map (juxt set identity) rules)
                              (apply concat)
                              (apply hash-map))]
    (map rule-set-to-rule pairs)))

(defn before-in? [pages [a b]]
  (< (find-index pages a)
     (find-index pages b)))

(defn correctly-ordered? [rules pages]
  (every? #(before-in? pages %) (get-relevant-rules rules pages)))

(defn get-mid [pages]
  "Get the middle of a pages vector"
  (let [mid (-> (count pages)
                (/ 2)
                double
                Math/round
                dec)]
    (get pages mid)))

(defn process [{:keys [rules updates]}]
  (->> (filter (partial correctly-ordered? rules) updates)
       (map get-mid)
       (map #(Integer/parseInt %))
       (apply +)))

(defn solve [input]
  (->> input
       format-input
       process))

(defn -main
  "Run with (n)bb -m aoc24.day05.part-1"
  [& args]
  (->> input 
       solve
       prn))


(comment

  (def test-input

    "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

  

  (def formatted-test-input (let [[rules updates] (format-input test-input)]
                              {:rules rules :updates updates}))
  

  (->> (combinations ["75" "47" "61" "53" "29"] 2)
       (map set))
  

  ((fn check-for-rules [rules pairs]
     (let [rule-set-to-rule (->> (map (juxt set identity) rules)
                                 (apply concat)
                                 (apply hash-map))]
       (map rule-set-to-rule pairs)))
   (->> test-input
        format-input
        :rules)
   (->> (combinations ["75" "47" "61" "53" "29"] 2)
        (map set)))

   (get-relevant-rules (->> test-input
                            format-input
                            :rules)
                       ["75" "47" "61" "53" "29"])

   (find-index ["75" "47" "61" "53" "29"] "53")

   (->> [["75" "47" "61" "53" "29"] ["75" "47"]]
        (apply
         (fn before-in? [pages [a b]]
           (< (find-index pages a)
              (find-index pages b)))))

   (->> [["75" "97" "47" "61" "53"] ["97" "75"]]
        (apply
         (fn before-in? [pages [a b]]
           (< (find-index pages a)
              (find-index pages b)))))

   (let [pages ["75" "97" "47" "61" "53"]
         rules (get-relevant-rules (:rules formatted-test-input) pages)]
     rules
     (every? #(before-in? pages %) rules))

   (correctly-ordered? (:rules formatted-test-input) ["75" "97" "47" "61" "53"])
   (correctly-ordered? (:rules formatted-test-input) ["75" "47" "61" "53" "29"])

   (filter (partial correctly-ordered? (:rules formatted-test-input)) (:updates formatted-test-input))
   
   
   (as-> 
   ;["75" "47" "61" "53" "29"] 
    ["61" "13" "29"]
    pages
     (get pages (-> (count pages)
                    (/ 2)
                    double
                    Math/round
                    dec)))
   
   
   
   
   
   (get-mid ["97" "61" "53" "29" "13"])

   (defn test-main []
     (->> test-input
          format-input
          process))
   )