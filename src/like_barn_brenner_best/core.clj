(ns like-barn-brenner-best.core
  (:require [clj-obt.core :as obt]
            [clj-obt.tools :as t])
  (:gen-class))

(obt/set-obt-path! "/home/ogrim/bin/The-Oslo-Bergen-Tagger/")

(def ordtak
  ["Bedre med trøtte armer enn tomme tarmer"
   "Den som er redd for å få vondt i armene, får vondt i tarmene"
   "Den som ikke i hete vil svette, får i kulde sulte"
   "Ingen er så brukbar som poteten"
   "Slit og møde gir klær og føde"
   "Denne uka gikk fort, i overmorgen er det alt onsdag, sa mannen"
   "Lediggang er roten til alt vondt"
   "Nye skjorter koster"
   "Arbeid tiltaler meg, jeg kan se på det i timesvis"
   "Hvorfor gjøre i morgen, det man kan gjøre i dag"
   "Kvinner ler når de vil, og gråter når de kan"
   "Nød lærer naken kvinne å spinne"
   "Uten mat og drikke duger helten ikke"
   "Bedre føre var, enn etter snar"
   "Borte bra, men hjemme best"
   "Like barn leker best"
   "Brent barn skyr ilden"
   "Det er ikke så farlig om pølsa er for lang"
   "Bedre med tørr kake enn intet å smake"
   "Det fins verre mat i verden enn raspeballer"
   "Bare bok gjør ingen klok"
   "Den lekse man lærer som ung, er aldri tung"
   "Det er bedre å komme fem minutter for sent enn å daue førti år for tidlig"
   "Det er ille å drukne på tørre landet"
   "En ulykke kommer sjelden alene"
   "Liten tue, kan velte stort lass"
   "Det er mangt stygt i havet, sa mannen, han så kjerringa svømme naken"
   "Det er ikke gull alt som glitrer"
   "Eple faller ikke langt fra stammen"
   "Det er bedre med en fugl i hånden enn ti på taket"])

(def split-tags ["inf-merke" "prep" "konj" "<komma>" "verb"])

(def initial-population (obt/obt-tag ordtak))

(defn random-bag [n bag-size]
  {:pre [(<= n bag-size)]}
  (->> (range bag-size) shuffle (take n)))

(defn parsed->string [parsed]
  (->> (map #(str (:word %) " ") parsed) (apply str) .trim))

(defn split [parsed tag]
  (let [result (t/filter-tag parsed tag)]
    (if (empty? result) nil (first result))))

(defn all-splits [parsed]
  (->> (map (partial split parsed) split-tags)
       (remove nil?)))

(defn split-by [parsed tag]
  (split-at (dec (:i tag)) parsed))

(defn fitness-function [individual]
  (* (java.lang.Math/tanh (count (all-splits individual))) (count individual)))

(defn sort-population [population]
  (reverse (sort-by fitness-function population)))

(defn random-split [population]
  (let [[ind1 ind2] (random-bag 2 (count population))
        i1 (nth population ind1)
        i2 (nth population ind2)
        [s1a s1b] (split-by i1 (rand-nth (all-splits i1)))
        [s2a s2b] (split-by i2 (rand-nth (all-splits i2)))]
    (map parsed->string [(flatten [s1a s2b]) (flatten [s2a s1b])])))

(defn generation [population]
  (let [new-individuals (mapcat (fn [x] (random-split initial-population))
                                (range (count population)))
        new-parsed (obt/obt-tag new-individuals)
        combination (sort-population (concat new-parsed population))]
    (take (count population) combination)))

(defn run-iterations [n]
  (loop [i 0 , population initial-population]
    (if (= i n) population
        (recur (inc i) (generation initial-population)))))

(defn -main []
  (let [[s1 s2] (random-split initial-population)]
    (println s1)
    (println s2)
    (println)
    (read-line)
    (recur)))
