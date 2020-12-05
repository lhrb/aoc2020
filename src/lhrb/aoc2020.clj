(ns lhrb.aoc2020
  (:gen-class)
  (:require [clojure.string :as str]))

(comment
  ;; day 1
  (require '[clojure.math.combinatorics :as c])

  (def nums
    (->> (slurp "resources/day1")
        (str/split-lines)
        (map #(Long/valueOf %))))

  (defn f [x]
    (->> x
         (filter #(= 2020 (apply + %)))
         (map #(apply * %))))

  ;; riddle 1
  (f (c/combinations nums 2))
  ;; riddle 2
  (f (c/combinations nums 3)) )

(comment
  ;; day 2
  (require '[instaparse.core :as insta] )

  (def grammar
  (insta/parser
   "pwp = min <minus> max <separator> letter <separator> password
    min = #'\\d+'
    max = #'\\d+'
    minus = '-'
    letter = #'[a-zA-Z]'
    password = #'\\w*'
    separator = ' ' | ': '"))

  (grammar "16-19 x: bxxxxxxxxxxxxxxxxxx")

  (defn valid-pw? [m]
    (let [pattern (->> (str/split (:password m) #"")
                       (filter #(= % (:letter m)))
                       count)]
      (and (<= (:min m) pattern) (>= (:max m) pattern))))

  (defn xor [a b]
    (or (and (not a) b) (and a (not b))))

  (defn valid-pw2? [m]
    (let [pattern (str/split (:password m) #"")
          letter (:letter m)]
      (xor (= letter (get pattern (- (:min m) 1)))
           (= letter (get pattern (- (:max m) 1))))))

  (def pws
    (->>
     (str/split-lines (slurp "resources/day2"))
     (map grammar)
     (map rest)
     (map #(reduce (fn [m [k v]] (assoc m k v)) {} %))
     (map (fn [m] (-> m
                      (update :min #(Long/valueOf %))
                      (update :max #(Long/valueOf %)))))))

  ;; riddle1
  (->> pws
       (filter valid-pw?)
       count)

   (->> pws
       (filter valid-pw2?)
       count)

  )

(comment
  ;; day 3

  (def m
    (->>
     (str/split-lines (slurp "resources/day3"))
     (map #(str/split % #""))))

  (defn trees [m right down]
    (let [height (count m)
          width (count (first m))
          y (range 0 height down)
          x (->> (range 0 (* right height) right)
                 (map #(mod % width)))
          coords  (map vector y x)]
      coords
      (->> coords
           (map #(let [[x y] %]
                   (-> m
                       (nth x)
                       (nth y))))
           (filter #(= % "#"))
           count)))

  ;; riddle 1
  (trees m 3 1)

  ;; riddle 2

  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map #(let [[r d] %] (trees m r d)))
       (apply *))
)

(comment
  ;; day 4

  (require '[instaparse.core :as insta])
  (require '[clojure.spec.alpha :as s])

  (def grammar
    (insta/parser
     "pp = (entry <separator?>)+
      entry = field <separator> data
      field = #'[a-z]{3}'
      data = #'[a-zA-Z0-9#]*'
      separator = ':' | ' '"))

  (def passports
    (->> (slurp "resources/day4")
         (str/split-lines)
         (partition-by empty?)
         (remove #(empty? (first %)))
         (map #(reduce (fn [acc e] (str acc " " e)) %))
         (map #(grammar %))
         (map #(->> %
                    rest
                    (reduce (fn [acc [_ [_ key] [_ data]]]
                              (assoc acc (keyword key) data))
                            {})))))

 ; riddle 1
  (s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                            :opt-un [::cid]))

  (->>
   passports
   (filter #(s/valid? ::passport %))
   count)

  ; riddle 2
  (s/def ::byr #(re-matches #"19[2-9][0-9]|200[0-2]" %))
  (s/def ::iyr #(re-matches #"201[0-9]|2020" %))
  (s/def ::eyr #(re-matches #"20(2[0-9]|30)" %))
  (s/def ::hgt #(re-matches #"1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in" %))
  (s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
  (s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
  (s/def ::pid #(re-matches #"[0-9]{9}" %))

  (->>
   passports
   (filter #(s/valid? ::passport %))
   count)
  )

(comment
  ;; day 5
  (require '[clojure.math.numeric-tower :as math])

  (def seats (-> (slurp "resources/day5")
                 (str/split-lines)))

  (defn search [interval sequence signal]
    (loop [[s e] interval
           rows sequence]
      (if (empty? rows)
        s
        (let [m (math/floor (/ (+ s e) 2))]
          (recur
           (if (= (first rows) signal)
             [s m]
             [(+ m 1) e])
           (rest rows))))))

  (defn seat-id [in]
    (let [s (->> (str/split in #"") (split-at 7))
          row (search [0 127] (first s) "F")
          col (search [0 7] (second s) "L")]
      (+ (* row 8) col)))

  ;; riddle 1
  (->> seats
       (map seat-id)
       (reduce max))


  ;; riddle 2
  (defn find-seat [sorted-ids]
    (->> (map vector
              (rest sorted-ids)
              (butlast sorted-ids))
         (filter #(= 2 (apply - %)))
         first
         second
         inc))

  (->> seats
       (map seat-id)
       sort
       find-seat)

  )
