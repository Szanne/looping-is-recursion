(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (= n 0)
                   acc
                   (recur (* acc base) (dec n))))]
    (if (= exp 0)
      1
      (helper base (dec exp)))))

(defn last-element [a-seq]
  (cond
   (empty? a-seq)
     nil
   (= 1 (count a-seq))
     (first a-seq)
   :else
     (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
        true
    (or (empty? seq1) (empty? seq2))
        false
    (= (first seq1) (first seq2))
        (recur (rest seq1) (rest seq2))
	:else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         coll a-seq]
    (cond
      (empty? coll)       nil
      (pred (first coll)) index
      :else               (recur (inc index) (rest coll)))))
;(+ 2 3)
;(find-first-index zero? [1 1 1 0 3 7 0 2])

;(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
;(find-first-index zero? [1 1 3 7 2])                          ;=> nil
;(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
;(find-first-index nil? [])

(defn avg [a-seq]
  (loop [n 0
         sum 0
         b-seq a-seq]
     (cond
      (and (empty? b-seq) (= n 0))
        0
      (empty? b-seq)
        (/ sum n)
      :else
        (recur (inc n) (+ sum (first b-seq)) (rest b-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [answer #{}
         b-seq a-seq]
    (if (empty? b-seq)
      answer
      (recur (toggle answer (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [n1 1
           n2 0
           n n]
      (if (= n 0)
        n2
        (recur n2 (+ n1 n2) (dec n))))))

(defn cut-at-repetition [a-seq]
  (loop [answer []
         answer-set #{}
         b-seq a-seq]
    (if (or (contains? answer-set (first b-seq)) (empty? b-seq))
      answer
      (recur
       (conj answer (first b-seq))
             (conj answer-set (first b-seq))
             (rest b-seq)))))
