(ns algebra.core
  (:refer-clojure :only [])
  (:require
   [fct.core]
   [clojure.core :as c]))


;;setup
(c/refer 'fct.core)


;;compare performance 
(c/defn test-loop [n f]
  (c/loop [n n]
    (if (c/= n 0)
      "done"
      (do (f)
          (recur (c/dec n))))))

(def term (var* :term (fn []
                        (rand-nth (map (fn [x] (str "term" x))
                                       (range 20))))))

(gen* (term))


;; elements are lists of terms, here generated with random length between 0 and 9
(def element (var* :element (fn [] (map (fn [x] (term))
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


(ftest* same-type-element)

;; add-term takes an element as argument and adds the corresponding terms if they have the same type as the first term  
(def add-term (var* :add-term (fn [e] {:gen (fn [] (vector (same-type-element)))}
                                (if-else (not (empty? e))
                                         (let [first-term (first e)
                                               type-check  (every? (fn [t] (com first-term t))
                                                                   e)]
                                           (if-else type-check
                                                    first-term
                                                    (fct.core/throw (Exception. "add-term can only deal with terms of the same type"))))                                             
                                         (fct.core/throw (Exception. "add-term cannot deal with empty list"))))))
 
(ftest* add-term)

(gcheck* add-term)


;; isolate the terms of a certain type in an element
(def find-same (fn find-same [t e] {:gen (fn [] (vector (term) (element)))}
                 {:same-type (filter (fn [a] (com t a)) e)
                  :remainder (filter (fn [a] (not (com t a))) e)}))

(ftest* find-same)
(gcheck* find-same)


;; simplify an element by adding all terms of the same type 
(def simplify (fn [e] {:gen (fn [] (vector (element)))} 
                (loop [x e
                       r []]
                  {:test (empty? x)
                   :rec (let [[t] x
                              {:keys [remainder same-type]} (find-same t x)
                              sum  (add-term same-type)
                              new-y (conj r sum)]
                          (rec remainder new-y))
                   :ret r})))

(gcheck* simplify)


(def add (fn [& elements] {:gen (fn [] (map (fn [x] (element))
                                            (range (rand-int 10))))}
           (simplify (apply concat elements))))

(ftest* add)

(gcheck* add)


;; multiplication of terms
(def mult-term (var* :mult-term (rand-fn element)))

(ftest* mult-term)

(def mult (fn [& elements] {:gen (fn [] (map (fn [x] (element))
                                              (range (rand-int 10))))}

            (let [simple-mult1 (fn [t e] {:gen (fn [] (vector (term) (element)))}
                                 (loop [e e r []]
                                   {:test (empty? e)
                                    :rec (let [[s] e]
                                           (rec (rest e) (concat (mult-term t s) r)))
                                    :ret (simplify r)}))
                  simple-mult  (fn [e1 e2] {:gen (fn [] (vector (element) (element)))}
                                 (loop [e1 e1 r []]
                                   {:test (empty? e1)
                                    :rec (let [[t] e1]
                                           (rec (rest e1)
                                                (concat (simple-mult1 t e2)
                                                        r)))
                                    :ret (simplify r)}))]
              
              (if-else (empty? elements)
                       []
                       (loop [elements (rest elements)
                              r (first elements)]
                         {:test (empty? elements)
                          :rec (let [[e] elements]
                                 (rec (rest elements) (simple-mult r e)))
                          :ret r})))))

(ftest* mult)

(defn power [e n]
  {:gen (fn [] (vector (element) (inc (rand-int 10))))}

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

(gcheck* power)

;; interpretion: polynomials 


(defn poly-vec [] 
  (map (fn [x] (rand-int 2))
       (range 2)))

(ftest* poly-vec)

(defn poly-term []
  (vector (rand-int 5) (poly-vec)))

(ftest* poly-term)

(defn poly-com [x y] {:gen (fn [] (vector (poly-term) (poly-term)))}
  (= (second x) (second y)))

(ftest* poly-com)

(defn poly-element []
  (map (fn [x] (poly-term))
       (range (rand-int 3))))

(ftest* poly-element)

(def poly-same-type-element (on-obj* same-type-element (atom {:term poly-term
                                                              :compare poly-com
                                                              :element poly-element})))

(ftest* poly-same-type-element)

(defn poly-add-term [t] {:gen (fn [] (vector (poly-same-type-element)))}
  (vector (apply + (map first t)) 
          (second (first t))))

(ftest* poly-add-term)

(def poly-add (on-obj* add (atom {:term poly-term 
                                  :element  poly-element 
                                  :compare  poly-com 
                                  :add-term  poly-add-term})))

(gcheck* poly-add)

(defn poly-mult-term [x y] {:gen (fn [] (vector (poly-term) (poly-term)))}
  (list (vector (* (first x) (first y))
                (map + (second x) (second y)))))

(gcheck* poly-mult-term)

(def poly-mult (on-obj* mult (atom {:term poly-term 
                                    :element  poly-element 
                                    :compare  poly-com 
                                    :add-term  poly-add-term
                                    :mult-term poly-mult-term})))

(def ev-poly-mult (gen* poly-mult))

(check* ev-poly-mult)

(gen* (let [a (poly-element)
            b (poly-element)]
        (list a (str "*") b (str "=") (poly-mult a b))))


