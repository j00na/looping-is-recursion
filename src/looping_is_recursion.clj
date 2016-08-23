(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [b-seq a-seq index 0]
    (cond
      (empty? b-seq) nil
      (pred (first b-seq)) index
      :else (recur (rest b-seq) (inc index)))))

(defn avg [a-seq]
  (loop [b-seq a-seq acc 0 count 0]
    (if (empty? b-seq)
      (/ acc count)
      (recur (rest b-seq) (+ acc (first b-seq)) (inc count)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [b-seq a-seq my-set (set [])]
    (if (empty? b-seq)
      my-set
      (recur (rest b-seq) (toggle my-set (first b-seq))))))

(defn fast-fibo [n]
  (loop [n n previous 0 current 1]
    (cond
      (zero? n) previous
      :else (recur (dec n) current (+ current previous)))))

(defn cut-at-repetition [a-seq]
  (loop [b-seq a-seq my-seq []]
    (cond
      (empty? b-seq) my-seq
      (some #{(first b-seq)} my-seq) my-seq
      :else (recur (rest b-seq) (conj my-seq (first b-seq))))))
