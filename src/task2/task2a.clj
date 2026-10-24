(ns task2.task2a)


(defn extend-one [chars prefix acc]
  (let [lastc (last prefix)]
    (if (empty? chars)
      acc
      (if (or (nil? lastc) (not= (first chars) lastc))
        (recur (rest chars) prefix (conj acc (str prefix (first chars))))
        (recur (rest chars) prefix acc)))))

(defn extend-all [strings chars acc]
  (if (empty? strings)
    acc
    (recur (rest strings) chars (extend-one chars (first strings) acc))))

(defn gen-core [chars remaining strs]
  (if (zero? remaining)
    strs
    (recur chars (dec remaining) (extend-all strs chars '()))))

(defn generate-strings [chars n]
  (if (zero? n)
    '("") 
    (gen-core chars n '(""))))


(generate-strings [\a \b] 0)
;; => ("")

(generate-strings [\a \b \c] 2)
;; => ("ac" "ab" "bc" "ba" "cb" "ca")

(generate-strings [\a \b \c \d] 2)
;; => ("ad" "ac" "ab" "bd" "bc" "ba" "cd" "cb" "ca" "dc" "db" "da")

(generate-strings [\a \b \c] 3)
;; => ("cac" "cab" "cbc" "cba" "bac" "bab" "bcb" "bca" "abc" "aba" "acb" "aca")
