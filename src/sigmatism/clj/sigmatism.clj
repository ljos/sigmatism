(ns sigmatism.clj.sigmatism
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
  (let [[fun & args] e]
    (cond (= 't (atom e))
          (assoc e a)

          (= 't (atom fun))
          (cond
           (= fun 'quote) (first args)
           (= fun 'atom)  (atom (eval (first args) a))
           (= fun 'eq)    (eq (eval (first args) a)
                              (eval (second args) a))
           (= fun 'car)   (first (eval (first args) a))
           (= fun 'cdr)   (rest (eval (first args) a))
           (= fun 'cons)  (cons (eval (first args) a)
                                (eval (second args) a))
           (= fun 'cond)  (evcon args a)
           :else
           (eval (cons (assoc fun a) args) a))

          (= (first fun) 'label)
          (let [[name body] (rest fun)]
            (eval (cons body args)
                  (cons (list name fun) a)))

          (= (first fun) 'lambda)
          (let [[largs body] fun]
            (eval body (append (pair largs (evlis args a)) a))))))
