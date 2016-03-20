(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [acc k]
              (cond (zero? k) 1
                    (= 1 k) acc
                    :else (recur (* acc base) (dec k))))]
    (pow base exp)))

(defn last-element [a-seq]
  (let [last (fn [candidate the-seq]
               (if (empty? the-seq)
                 candidate
                 (recur (first the-seq) (rest the-seq))))]
    (last nil a-seq)))

(defn seq= [seq1 seq2]
  (let [equal (fn [value a-seq b-seq]
                (cond (not value) value
                      (empty? a-seq) (empty? b-seq)
                      (empty? b-seq) false
                      :else (recur (= (first a-seq) (first b-seq)) (rest a-seq) (rest b-seq))))]
    (equal true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         the-seq a-seq]
    (cond (empty? the-seq) nil
          (pred (first the-seq)) index
          :else (recur (inc index) (rest the-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         the-seq a-seq]
    (if (empty? the-seq)
      (/ sum (max count 1))
      (recur (+ sum (first the-seq)) (inc count) (rest the-seq)))))

(defn parity [a-seq]
  (loop [parity-set #{}
         the-seq a-seq]
    (let [toggle (fn [set elem]
                   (if (contains? set elem)
                     (disj set elem)
                     (conj set elem)))]
      (if (empty? the-seq)
        parity-set
        (recur (toggle parity-set (first the-seq)) (rest the-seq))))))

(defn fast-fibo [n]
  (loop [fn-1 1
         fn 0
         k 0]
    (if (>= k n)
      fn
      (recur fn (+ fn-1 fn) (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [elems #{}
         result []
         the-seq a-seq]
    (let [first (first the-seq)]
      (if (or (contains? elems first) (empty? the-seq))
        result
        (recur (conj elems first) (conj result first) (rest the-seq))))))

