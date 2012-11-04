(ns sigmatism.clj.core
  (:refer-clojure :exclude [eval atom assoc not]))

(defn eq [x y]
  (cond (and (= x '()) (= y '()))
        't    
        (and (symbol? x) (symbol? y) (= x y))
        't     
        :else
        '()))

(defn assoc [x y]
  (if (= 't (eq (ffirst y) x))
    (-> y first fnext)
    (assoc x (rest y))))

(declare eval)

(defn evcon [c a]
  (if (= 't (eval (ffirst c) a))
    (eval (-> c first fnext) a)
    (evcon (rest c) a)))

(defn null [x]
  (eq x '()))

(defn append [x y]
  (if (= 't (null x))
    y
    (cons (first x) (append (rest x) y))))

(defn not [x]
  (if (= 't x)
    '()
    't))

(defn atom [x]
  (if (or (symbol? x)
          (= '() x))
    't
    '()))

(defn pair [x y]
  (cond (and (= 't (null x)) (= 't (null y)))
        '()
        
        (and (= 't (not (atom x))) (= 't (not (atom y))))
        (cons (list (first x) (first y))
              (pair (rest x) (rest y)))))

(defn evlis [m a]
  (if (= 't (null m))
    '()
    (cons (eval (first m) a)
          (evlis (rest m) a))))

(defn eval [e a]
  (cond (= 't (atom e))
        (assoc e a)
        
        (= 't (atom (first e)))
        (cond
         (= 't (eq (first e) 'quote)) (fnext e)
         (= 't (eq (first e) 'atom))  (atom (eval (fnext e) a))
         (= 't (eq (first e) 'eq))    (eq (eval (fnext e) a)
                                          (eval (first (nnext e)) a))
         (= 't (eq (first e) 'car))   (first (eval (fnext e) a))
         (= 't (eq (first e) 'cdr))   (rest (eval (fnext e) a))
         (= 't (eq (first e) 'cons))  (cons (eval (fnext e) a)
                                            (eval (first (nnext e)) a))
         (= 't (eq (first e) 'cond))  (evcon (rest e) a)
         :else
         (eval (cons (assoc (first e) a)
                     (rest e))
               a))
        
        (= 't (eq (ffirst e) 'label))
        (eval (cons (-> e first nnext first) (rest e))
              (cons (list (-> e first fnext) (first e)) a))

        (= 't (eq (ffirst e) 'lambda))
        (eval (-> e first nnext first)
              (append (pair (-> e first fnext) (evlis (rest e) a))
                      a))))
