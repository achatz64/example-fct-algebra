(ns algebra.core
  (:refer-clojure :only [])
  (:require
   [fct.core :as f]
   [clojure.core :as c]))


;;setup
(c/refer 'fct.core)


(def term (var* :term (fn [] 
                        (rand-nth (map (fn [x] (str "term" x))
                                       (range 20))))))

(gen* (term))

;; elements are lists of terms, here generated with random length between 0 and 9
;; although term is a function with no argument it just ignores arguments and does not throw an arrity exception
(def element (var* :element (fn [] (map term
                                        (range (rand-int 10))))))

(gen* (element))
 

;; decide whether two terms have the same type, we want the same object to have the same type
(def com (var* :compare (let [boolean (fn [] (rand-nth (list true false)))
                              some (rand-fn boolean)]
                          (fn [x y] {:gen (fn [] (vector (term) (term)))}
                            (if-else (= x y)
                                     true
                                     (some x y))))))

(ftest* com)



;; constructing elements with terms of type agreeing with the first term
(def same-type-element (fn [] (let [e (conj (element) (term))]
                                (filter (fn [t] (com (first e) t))
                                        e))))


;; add-term takes an element as argument and adds the corresponding terms if they have the same type as the first term  
(def add-term (var* :add-term (fn [e] {:gen (fn [] (vector (same-type-element)))}
                                (if-else (not (empty? e))
                                         (let [first-term (first e)
                                               type-check  (every? (fn [t] (com first-term t))
                                                                   e)]
                                           (if-else type-check
                                                    first-term
                                                    (list com e)))                                             
                                         (fct.core/throw (Exception. "cannot deal with empty list"))))))

(ftest* add-term)

;; isolate the terms of a certain type in an element
(def find-same (fn find-same [t e] {:gen (fn [] (vector (term) (element)))}
                 (hash-map :same-type (filter (fn [a] (com t a)) e)
                           :remainder (filter (fn [a] (not (com t a))) e))))

(ftest* find-same)


;; simplify an element by adding all terms of the same type 
(def simplify (fn [e] {:gen (fn [] (vector (element)))} 
                (loop [x e
                       r []]
                  {:test (empty? x)
                   :rec (let [[t] x
                              {:keys [remainder same-type]} (find-same t x)
                              sum (add-term same-type)
                              new-y (conj r sum)]
                          (rec remainder new-y))
                   :ret r})))

;(ftest* simplify)

;; we could also use (s/coll-of element) 
(def add (fn [& elements] {:gen (fn [] (map element
                                             (range (rand-int 10))))}
           (simplify (apply concat elements))))

(ftest* add)


;; multiplication of terms
(def mult-term (var* :mult-term (rand-fn element)))


(def mult (fn [& elements] {:gen (fn [] (map element
                                              (range (rand-int 10))))}

            (let [simple-mult1 (fn [t e] {:gen (fn [] (vector (term) (element)))}
                                 (loop [e e r []]
                                   {:test (empty? e)
                                    :rec (let [[s] e]
                                      (rec (rest e) (concat (mult-term t s) r)))
                                    :ret (simplify r)}))
                  
                  simple-mult (fn [e1 e2] {:gen (fn [] (vector (element) (element)))}
                                (loop [e1 e1 r []]
                                  {:test (empty? e1)
                                   :rec (let [[t] e1]
                                          (rec (rest e1) (concat (simple-mult1 t e2) r)))
                                   :ret (simplify r)}))]
              
              (if-else (empty? elements)
                       []
                       (loop [elements (rest elements)
                              r (first elements)]
                         {:test (empty? elements)
                          :rec (let [[e] elements]
                                 (rec (rest elements) (simple-mult r e)))
                          :ret r})))))



(defn power [e n]
  {:gen (fn [] (vector (element) (inc (rand-int 1000))))}

  (loop
      [f e
       n n
       r []
       first-time true]

    {:test (= n 1)

     :rec (cond
            (odd? n) (rec (mult f f) (/ (dec n) 2) (if-else first-time
                                                           e
                                                           (mult r e))
                         false)
           (even? n) (rec (mult f f) (/ n 2) r)
           :else first-time)
     
     :ret (if first-time
            f
            (mult f r))}))

(ftest* power)


;; interpretion: polynomials 


(defn poly-vec []
  (map (fn [] (rand-int 2)) (range 2)))

(defn poly-term []
  (vector (rand-int 5) (poly-vec)))

(defn poly-com [x y] {:gen (fn [] (vector (poly-term) (poly-term)))}
  (= (second x) (second y)))

(ftest* poly-com)

(defn poly-element []
  (map poly-term (range (rand-int 3))))

(def poly-same-type-element (sub* same-type-element {:term poly-term
                                                     :compare poly-com
                                                     :element poly-element}))

(defn poly-add-term [t] {:gen (fn [] (vector (poly-same-type-element)))}
  (vector (apply + (map first t)) 
          (second (first t))))

(ftest* poly-add-term)

(def poly-add (sub* add {:term poly-term 
                         :element  poly-element 
                         :compare  poly-com 
                         :add-term  poly-add-term}))

(gcheck* poly-add)

(defn poly-mult-term [x y] {:gen (fn [] (vector (poly-term) (poly-term)))}
  (list (vector (* (first x) (first y))
                (map + (second x) (second y)))))

(gcheck* poly-mult-term)

(def poly-mult (sub* mult {:term poly-term 
                           :element  poly-element 
                           :compare  poly-com 
                           :add-term  poly-add-term
                           :mult-term poly-mult-term}))

(def ev-poly-mult (ev* poly-mult {}))

(check* ev-poly-mult)

(gen* (let [a (poly-element)
            b (poly-element)]
        (list a (str "*") b (str "=") (poly-mult a b))))


