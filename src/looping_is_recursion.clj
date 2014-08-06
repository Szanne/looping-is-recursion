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
        (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         seqi a-seq]
    (if (empty? seqi)
      nil
      (if (pred (first seqi))
        n
        (recur (inc n) (rest a-seq))))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])

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

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [answer #{}
         b-seq a-seq]
    (if (empty? b-seq)
      answer
      (recur (toggle answer (first b-seq)) (rest b-seq)))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4] ;=> #{2 4}

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [n1 0
           n2 1
           n n]
      (if (= n 0)
        n2
        (fast-fibo n2 (+ n1 n2) (dec n))))))

        (fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  (loop [answer []
         answer-set #{}
         b-seq a-seq]
    (if (contains? answer (first b-seq))
      (answer)
      (cut-at-repetition
       (conj answer (first b-seq)
             (conj answer-set (first b-seq))
             (rest b-seq))))))

        (cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]

