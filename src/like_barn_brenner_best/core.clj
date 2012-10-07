(ns like-barn-brenner-best.core
  (:require [clj-obt.tools :as t])
  (:use [like-barn-brenner-best.proverbs]))

(def split-tags ["inf-merke" "prep" "konj" "<komma>" "verb"])

(defn random-bag [n bag-size]
  {:pre [(<= n bag-size)]}
  (->> (range bag-size) shuffle (take n)))

(defn parsed->string [parsed]
  (->> parsed
       (map #(str (if (t/in? (:tags %) "<komma>") "" " ") (:word %)))
       (apply str)
       .trim))

(defn split [parsed tag]
  (let [result (t/filter-tag parsed tag)]
    (if (empty? result) nil (first result))))

(defn all-splits [parsed]
  (->> (map (partial split parsed) split-tags)
       (remove nil?)))

(defn split-by [parsed tag]
  (split-at (dec (:i tag)) parsed))

(defn random-split [population]
  (let [[ind1 ind2] (random-bag 2 (count population))
        i1 (nth population ind1)
        i2 (nth population ind2)
        [s1a s1b] (split-by i1 (rand-nth (all-splits i1)))
        [s2a s2b] (split-by i2 (rand-nth (all-splits i2)))]
    [(flatten [s1a s2b]) (flatten [s2a s1b])]))

(defn random-proverb []
  (first (random-split ordtak-tagged)))

(defn permutate-left [proverb splits]
  (let [[_ back] (split-by proverb (rand-nth splits))
        randp (rand-nth ordtak-tagged)
        [front] (split-by randp (rand-nth (all-splits randp)))]
    (flatten [front back])))

(defn permutate-right [proverb splits]
  (let [[front] (split-by proverb (rand-nth splits))
        randp (rand-nth ordtak-tagged)
        [_ back] (split-by randp (rand-nth (all-splits randp)))]
    (flatten [front back])))
