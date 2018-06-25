(in-package :unification)

(assert
 (string= (with-standard-io-syntax
            (with-output-to-string (out)
              (with-logical-variables (?a)
                (let ((*occur-check* nil))
                  (unify ?a (list ?a)))
                (write ?a :stream out :circle t :pretty t))))
          "#1=(#1#)")) 

(assert
 (unify '(3 2 1)
        (with-logical-variables (?a ?b)
          (unify `(,?a . (2 1)) `(3 . ,?b)))))

(assert
 (eq :warning
     (handler-case
         (term-information #(0 1 2 3)
                           :ignore-ground-terms t
                           :ignore-variables t
                           :visitor (lambda (item &key walker)
                                      (prog1 :atomic
                                        (typecase item
                                          (array (map () walker item))))))
       (warning () :warning)
       (:no-error () :normal))))

(block nil
  (with-logical-variables (?a ?b)
    (multiple-value-bind (category vars ground)
        (term-information `(let ((,?a 3)) (list (if ,?b
                                                    (+ 3 2)
                                                    (* 4 5))
                                                (a b (c d))
                                                ,?b
                                                (a b (c d))
                                                ?b)))
      ;; (return (list category vars ground))
      (assert (eq :compound category))
      (assert (= 2 (hash-table-count vars)))
      (assert (= 4 (hash-table-count ground)))
      (assert (= 1 (gethash ?a vars)))
      (assert (= 2 (gethash ?b vars)))
      (loop for (k . v) in 
            '(((A B (C D)) . 1)
              (((A B (C D)) ?B) . 1)
              (((+ 3 2) (* 4 5)) . 1)
              ((3) . 1))
            do (assert (= (gethash k ground) v))))))

(with-logical-variables (?a ?b ?c)
  (unify ?c 100)
  (let ((term `(let ((,?a 3))
                 (if ,?b
                     (+ ,?c 2)
                     (* 4 5)))))
    (assert (unify term (copy-term term)))))

(with-logical-variables (?a ?b)
  (unify `(x ,?a) `(,?b y))
  (assert (equal (list ?b ?a) '(x y))))

(with-logical-variables (?a ?b)
  (setf ?a ?b ?b 3)
  (assert (equal '(3 3) (list ?a ?b))))

(with-logical-variables (?a ?b ?c ?d)
  (shiftf ?a ?b ?c ?d 0)
  (assert (every #'zerop (list ?a ?b ?c ?d))))
