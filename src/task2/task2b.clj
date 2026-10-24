(ns task2.task2b)


(defn generate-strings-fp [chars n]
  (nth (iterate
        (fn [prefixes]
          (mapcat (fn [p]
                    (map (fn [c] (str p c))
                         (filter (fn [c] (or (empty? p) (not= c (last p))))
                                 chars)))
                  prefixes))
        '(""))
       n))


(generate-strings-fp [\a \b] 0)
;; => ("")

(generate-strings-fp [\a \b \c] 2)
;; => ("ac" "ab" "bc" "ba" "cb" "ca")

(generate-strings-fp [\a \b \c \d] 2)
;; => ("ad" "ac" "ab" "bd" "bc" "ba" "cd" "cb" "ca" "dc" "db" "da")

(generate-strings-fp [\a \b \c] 3)
;; => ("cac" "cab" "cbc" "cba" "bac" "bab" "bcb" "bca" "abc" "aba" "acb" "aca")