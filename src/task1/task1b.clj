(ns task1.task1b
  (:require [task1.terms :refer [variable
                                 lambda
                                 app
                                 variable?
                                 lambda?
                                 app?]]))


(defn naive-substitute [term x replacement]
  (cond
    (variable? term)
    (if (= (second term) x)
      replacement
      term)

    (lambda? term)
    (if (= (second term) x)
      term
      (lambda (second term)
              (naive-substitute (nth term 2) x replacement)))

    (app? term)
    (app (naive-substitute (second term) x replacement)
         (naive-substitute (nth term 2) x replacement))))


(naive-substitute
 (variable 'x)
 'x
 (variable 'y))
;; => [:var y]

(naive-substitute
 (variable 'y)
 'x
 (variable 'z))
;; => [:var y]

(naive-substitute
 (lambda 'x

         (variable 'x))
 'x
 (variable 'y))
;; => [:lambda x [:var x]]

(naive-substitute
 (lambda 'y
         (variable 'x))
 'x
 (variable 'y))
;; => [:lambda y [:var y]]
