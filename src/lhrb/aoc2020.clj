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
