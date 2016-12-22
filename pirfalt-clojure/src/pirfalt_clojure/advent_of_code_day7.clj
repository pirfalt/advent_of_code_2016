(ns pirfalt-clojure.advent_of_code-day7
  (:require [clojure.string :as string]
            [clojure.repl :as repl]))

(def input "abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn")

(def lines (string/split-lines input))

(def line (first lines))

(defn abba-in-seq [aseq]
  (loop [trail [nil nil nil]
         out false
         out' true
         cs aseq]
   (let [[a b c] trail
         d (first cs)]
     (cond
       (= d nil)                           out
       (= d \[)                            (recur [b c d] out false (rest cs))
       (= d \])                            (recur [b c d] out true (rest cs))
       (and (not= a b) (= b c) (= a d))    (if out'
                                            (recur [b c d] out' out' (rest cs))
                                            false)
      :else                                (recur [b c d] out out' (rest cs))))))


(defn problem-1 [lines]
 (count (filter true? (map abba-in-seq lines))))
  
(println "Problem 1: " 
  (problem-1 (string/split-lines (slurp "./resources/day7-input.txt"))))







(defn aba-finder [line]
 (loop [out []
        out' :none-bracketed
        trail [nil nil]
        cs line]
  (let [[a b] trail
        c (first cs)]
    (cond
      (= c nil)                           out
      (= c \[)                            (recur out :bracketed [nil nil] (rest cs))
      (= c \])                            (recur out :none-bracketed [nil nil] (rest cs))
      (and (not= a b) (= a c))            (recur (conj out [out' a b c]) out' [b c] (rest cs))
     :else                                (recur out out' [b c] (rest cs))))))


(let [abas (aba-finder "a[aba]brbr[h]uu")]
  (for [nb (filter #(= :none-bracketed (first %)) abas)
        any abas]
   (println [(and
              (= (nb 1) (any 2))
              (= (nb 2) (any 1))) nb any]))) 


(defn check-aba-match [abas]
    (some true? (for [nb (filter #(= :none-bracketed (first %)) abas)
                      any abas]
                 (and
                   (= (nb 1) (any 2))
                   (= (nb 2) (any 1))))))


(defn problem-2 [lines]
; (let [lines (string/split-lines "aba[bab]xyz
; xyx[xyx]xyx
; aaa[kek]eke
; zazbz[bzb]cdb")]
 (->> lines
  (map aba-finder)
  (map check-aba-match)
  (filter true?)
  (count)))
    

(println "Problem 2: "
  (problem-2 (string/split-lines (slurp "./resources/day7-input.txt"))))
