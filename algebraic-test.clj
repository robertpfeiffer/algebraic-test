(ns de.uni-potsdam.hpi.algebraic-test)

(defn cartp
  "cartesian product"
  ([] ())
  ([coll] (map list coll))
  ([coll & rest] (for [x coll y (apply cartp rest)] (conj y x))))

(defn same? [fun & rest]
  (apply = (map fun rest)))

(defn implies? [a b]
  (boolean (or (not a) b)))

(defn andf [& funs]
  (fn [& args]
      (every? #(apply % args) funs)))

;properties of relations
(defn symmetrical?
  [rel example-vals]
  (every? (fn [[a b]] (implies? (rel a b) (rel b a)))
	  (cartp example-vals example-vals)))

(defn antisymmetrical?
  [rel example-vals]
  (every? (fn [[a b]] (implies? (and (rel a b) (rel b a)) (= a b)))
	  (cartp example-vals example-vals)))

(defn reflexive?
  [rel example-vals]
  (every? #(rel % %) example-vals))

(defn transitive?
  [rel example-vals]
  (every? (fn [[a b c]] (implies? (and (rel a b) (rel b c)) (rel a c)))
	  (cartp example-vals example-vals example-vals)))

;special relations
(def equivalence?
     (andf transitive?
	   reflexive?
	   symmetrical?))

(def half-ordering?
     (andf transitive?
	   reflexive?
	   antisymmetrical?))

;properties of sets with operations
(defn closure-under? 
  [op vals check]
  (and (every? check vals)
       (every? (fn [[a b]] (check (op a b))) (cartp vals vals))))

(defn associative?
  [op example-vals]
  (every? (fn [[a b c]] (= (op a (op b c)) (op (op a b) c)))
	  (cartp example-vals example-vals example-vals)))

(defn commutative?
  [op example-vals]
  (every? (fn [[a b]] (= (op a b) (op b a)))
	  (cartp example-vals example-vals)))

(defn idempotent?
  [op example-vals]
  (every? #(= % (op % %)) example-vals))

(defn distributive?
  [op1 op2 example-vals]
  (every? (fn [[a b c]] (= (op1 (op2 a b) (op2 a c)) (op2 a (op1 b c))))
	  (cartp example-vals example-vals example-vals)))

;algebraic structures
(defn semigroup?
  ([op example-vals check]
  (and (closure-under? op example-vals check)
       (associative? op example-vals))))

(defn monoid?
  ([op neutral example-vals check]
  (and (= neutral (op neutral neutral))
       (every? (fn [val] (= val (op neutral val) (op val neutral)))
	       example-vals)
       (semigroup? op example-vals check))))

(defn group?
  [op inv neutral example-vals check]
  (let [example-vals (concat example-vals (map inv example-vals))]
    (and (= neutral (inv neutral))
	 (monoid? op neutral example-vals check)
	 (every? (fn [val]
                     (let [ival (inv val)] 
		       (and (= val (inv ival))
			    (= neutral (op val ival)
			       (op ival val)))))
		 example-vals))))

;mapping structures to other structures
(defn homomorphism?
  [struct1-vals op1 morph op2]
  (every? (fn [[a b]] (= (morph (op1 a b)) (op2 (morph b) (morph a))))
	  (cartp struct1-vals struct1-vals)))

(defn group-homomorphism?
  [group1-vals op1 inv1 neutral1
   morph       op2 inv2 neutral2]
  (and (= (morph neutral1) neutral2)
       (homomorphism? group1-vals op1 morph op2)
       (every? #(= (morph (inv1 %)) (inv1 (morph %))) group1-vals)))
