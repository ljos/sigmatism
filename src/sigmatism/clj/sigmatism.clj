(ns sigmatism.clj.sigmatism
  (:refer-clojure :exclude [eval atom]))

(defn eq [x y]
  (if (and (symbol? x) (symbol? y) (= x y)) 't 'nil))

(defn null [x]
  (eq x 'nil))

(defn atom [x]
  (if (symbol? x) 't 'nil))

(defn eval
  ([e]
     (eval e {}))
  ([e a]
     (if (= 't (atom e))
       (a e)
       (let [[fun & args] e]
         (cond (= 't (atom (first e)))
               (cond
                (= fun 'quote) (first args)
                (= fun 'atom)  (atom (eval (first args) a))
                (= fun 'eq)    (eq (eval (first args) a)
                                   (eval (second args) a))
                (= fun 'car)   (first (eval (first args) a))
                (= fun 'cdr)   (rest (eval (first args) a))
                (= fun 'cons)  (cons (eval (first args) a)
                                     (eval (second args) a))
                (= fun 'cond)  (loop [[[c ex] & conds] args]
                                 (if-not (nil? (eval c a))
                                   (eval ex a)
                                   (recur conds)))
                :else
                (eval (cons (a fun) args) a))

               (= (ffirst e) 'label)
               (let [[name body] (rest fun)]
                 (eval (cons body args) (assoc a name fun)))

               (= (ffirst e) 'lambda)
               (let [[largs body] (rest fun)]
                 (->> (map #(eval % a) args)
                      (interleave largs)
                      (apply (partial assoc a))
                      (eval body))))))))

(defn repl []
  (print "lambda> ")
  (flush)
  (-> (read)
      (eval)
      (println))
  (flush)
  (recur))
