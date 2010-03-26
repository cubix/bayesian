(ns bayesian
  (:use [clojure.contrib str-utils duck-streams]))

(defn- compr [acc elt n lst] 
  (if (empty? lst) 
    (conj acc {elt n})
    (let [next (first lst)] 
      (if (= next elt) 
	(recur acc elt (inc n) (rest lst)) 
	(recur (conj acc {elt n}) next 1 (rest lst)))))) 

(defn compress
  "Run length encoding. (Based the version in book ANSI Common Lisp.)"
  [x]
  (vec (compr [] (first x) 1 (rest x)))) 

(defn word-count
  "Creates a count of words by sorting and then compressing it."
  [lst]
  (apply merge (-> lst sort compress)))

(defn parse-file [file]
  (word-count 
   (filter #(re-matches #"[a-z]{3,44}" %)
	   (map (memfn toLowerCase)
		(re-split #"\s+" 
			  (slurp file))))))

(def words (ref (load-file "words.clj")))

(defn update-category [cat wc]
  (dosync (alter words assoc-in [cat] wc)))

(defn add-words [category words-in-file]
  (update-category category 
		   (merge-with + (@words category) words-in-file)))

(defn category-count 
  "Total number of words in given category."
  [db cat]
  (let [wc (db cat)]
   (reduce + (map (fn [[word c]] c) wc))))
(def category-count (memoize category-count))

(defn total-count
  "Total words across all categories."
  [db]
  (reduce + (map #(category-count db %)  (keys db))))
(def total-count (memoize total-count))

(defn cat-word [db cat word]
  ((db cat) word))

(defn word-score [db cat word]
  (Math/log10 (/ (or (cat-word db cat word) 0.01)
		 (category-count db cat))))

(defn category-score [db cat]
  (Math/log10 (/ (category-count db cat)
		 (total-count db))))

(defn classify
  "Get the classification of a file from word counts"
  [db words-in-file]
  (let [categories (keys db)
	word-list (keys words-in-file)]
    (apply merge-with +
	   (concat 
	    ; Run through words and calculate the probability for each category
	    (map (fn [word] 
		   (apply merge (map (fn [cat] 
				       (hash-map cat (word-score db cat word)))
					    categories))) 
		 word-list)
	    ; Add in the probability that the text is of a specific category
	    (map (fn [cat] (hash-map cat (category-score db cat)))
		 categories)))))

(defn main []
  (let [cmd (first *command-line-args*)]
    (cond
	(= cmd "add")
 	 (do (add-words (second *command-line-args*) 
			(parse-file (nth *command-line-args* 2)))
	     (spit "words.clj" @words))
        (= cmd "classify")
	 (classify @words (parse-file (second *command-line-args*))))))

(main)
