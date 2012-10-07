(ns like-barn-brenner-best.app
  (:use [like-barn-brenner-best.core]
        [like-barn-brenner-best.proverbs])
  (:gen-class))

(defn proverb-state [proverb]
  {:proverb proverb
   :splits (all-splits proverb)})

(def active-proverb
  (atom (proverb-state (rand-nth ordtak-tagged))))

(defn active->string [] (parsed->string (:proverb @active-proverb)))

(defn reset-proverb []
  (reset! active-proverb (proverb-state (rand-nth ordtak-tagged))))

(defn permutate-direction [& [direction]]
  (if (empty? (:splits @active-proverb)) (random-proverb)
      (case direction
        :left (permutate-left (:proverb @active-proverb) (:splits @active-proverb))
        :right (permutate-right (:proverb @active-proverb) (:splits @active-proverb))
        (ordinary-proverb))))

(defn permutate-active [& [direction]]
  (->> (proverb-state (permutate-direction direction))
       (reset! active-proverb))
  (parsed->string (:proverb @active-proverb)))


(comment
  (defn -main []
    (let [[s1 s2] (random-split ordtak-tagged)]
      (println s1)
      (println s2)
      (println)
      (read-line)
      (recur))))
