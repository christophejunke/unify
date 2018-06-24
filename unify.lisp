(in-package :unify)

(declaim (inline unify-var-term
                 value
                 unify%
                 (setf value)
                 %logical-variable-boundp
                 %replace-by-copies))

;;;; ERROR REPORTING

(defgeneric report-format (object)
  (:method-combination append :most-specific-last))

(defgeneric report-dispatch (object stream)
  (:method (object stream)
    (format stream "~&~{~@?~^ ~}" (report-format object))))

;;;; ERRORS

(define-condition unification-error ()
  ((%first-term :initarg :first :reader %first-term)
   (%second-term :initarg :second :reader %second-term))
  (:documentation "Error related to unification of two terms")
  (:report report-dispatch))

(defmethod report-format append ((condition unification-error))
  (list "Cannot unify ~s and ~s."
        (%first-term condition)
        (%second-term condition)))

(define-condition occurrence-error (unification-error)
  ((%first-term :initarg :var :reader %var)
   (%second-term :initarg :term :reader %term))
  (:documentation "Unification error due to occurence check.")
  (:report report-dispatch))

(defmethod report-format append ((condition occurrence-error))
  (list "Variable ~a occurs in term ~a."
        (%var condition)
        (%term condition)))

;;;; VARIABLES

(defgeneric value (value-or-variable)
  (:method (value) value)
  (:documentation
   "Return the most precise value known for VALUE-OR-VARIABLE.

    If VALUE-OR-VARIABLE is of type LOGICAL-VARIABLE, either return
    the variable, if it is currently unbound, or its value if the
    variable is instantiated. For all other value, return the value
    itself."))

(defclass logical-variable ()
  ((name :accessor name
         :initarg :name
         :initform (gensym "?"))
   (%%value :reader value
            :writer %set-logical-variable-value
            :initarg :value)))

(defun %logical-variable-bound-p (variable)
  "NOT recursive."
  (slot-boundp variable '%%value))

(defun %make-logical-variable-unbound (var)
  (slot-makunbound var '%%value))

(defgeneric copy-variable (logical-variable &key &allow-other-keys)
  (:method ((variable logical-variable) &key &allow-other-keys)
    (make-instance 'logical-variable)))

(defmethod value :around ((variable logical-variable))
  (if (%logical-variable-bound-p variable)
      (value (call-next-method))
      variable))

(defvar *logical-variable-class*
  (find-class 'logical-variable)
  "Class to use when creating logic variables")

(defun var (&rest args)
  (apply #'make-instance *logical-variable-class* args))

;; TODO when unification fails, unbind (see backtracking.lisp)
;; TODO More docs

(defgeneric unify (first-term second-term)
  (:method (first second)
    "Fail by default"
    (error 'unification-error
           :first first
           :second second))
  (:method :around (first second)
    "Always return first term, trivially unify identical terms"
    (prog1 first
      (unless (eql first second)
        (call-next-method))))
  (:documentation
   "Unify two terms by instantiating variables, if possible."))

(defun (setf value) (term var)
  "Another way to unify VAR with TERM"
  (unify var term))

;;;; PRETTY PRINTING

;; TODO: print readably
;; (reading back should unify all occurrences of the same variable)
;; TODO: make-load-form (?)

(defmethod print-object ((variable logical-variable) stream)
  ""
  (with-accessors ((name name) (value value)) variable
    (flet ((emit ()
             (cond
               ((%logical-variable-bound-p variable)
                (write value :stream stream))
               (:otherwise
                (format stream "~S" name)))))
      (if *print-pretty*
          (emit)
          (print-unreadable-object (variable stream :type t)
            (write (name variable) :stream stream)
            (write #\space :stream stream :escape nil)
            (emit))))))

;;;; OCCURRENCE CHECK

(defparameter *occur-check* t
  "Whether to perform an occur check before unifying terms.

Disabling occurrence check might lead to circular data structures. If
you bind *OCCUR-CHECK* to NIL, it might be a good idea to also bind
*PRINT-CIRCLE* to T.")

(defgeneric occurs (value term)
  (:documentation
   "Return non-NIL if VALUE occurs in TERM.

Default methods only knows how to recurse into cons cells. It is thus
recommended to specialize this function for custom data-structures.")
  (:method (value term)
    (eq value term))
  (:method (value (var logical-variable))
    (or (call-next-method)
        (and (%logical-variable-bound-p var)
             (occurs value (value var)))))
  (:method (value (expr cons))
    (or (call-next-method)
        (occurs value (car expr))
        (occurs value (cdr expr)))))

(defun check-occurrence (var term)
  "Errors with OCCURRENCE-ERROR if VAR occurs in TERM.

Occurrence check is performed before unification to detect when a
variable ?V is unified to a term which contains a reference to ?V. For
example, the following unification fails when *OCCUR-CHECK* is
non-NIL:

    (UNIFY ?V (LIST ?V))

The reason behind this check is to prevent the creation of circular
data-structures. It is possible to disable the automatic check by
letting *OCCUR-CHECK* be NIL."
  (when (occurs var term)
    (error 'occurrence-error
           :var var
           :term term)))

(defun unify-var-term (var term)
  (cond
    ((%logical-variable-bound-p var)
     (unify (value var) term))
    (t (when *occur-check*
         (check-occurrence var term))
       (%set-logical-variable-value term var))))

(defmethod unify ((v1 logical-variable) (v2 logical-variable))
  (cond
    ((%logical-variable-bound-p v1) (unify (value v1) v2))
    ((%logical-variable-bound-p v2) (unify v1 (value v2)))
    (t (call-next-method))))

(defmethod unify ((var logical-variable) term)
  (unify-var-term var term))

(defmethod unify (term (var logical-variable))
  (unify-var-term var term))

(defmethod unify ((expr1 cons) (expr2 cons))
  (unify (car expr1) (car expr2))
  (unify (cdr expr1) (cdr expr2)))

(defgeneric compute-term-information (term &key walker)
  (:method (otherwise &key &allow-other-keys) :atomic)
  (:method ((cons cons) &key walker)
    (funcall walker
             (car cons)
             (cdr cons))))

(defun term-information
    (term &key
            (ignore-variables nil)
            (ignore-ground-terms nil)
            (debugp nil)
            (visitor #'compute-term-information))
  "Walk a term and extract information about variables and ground terms.

TERM is a Common Lisp value that might hold instances of
LOGICAL-VARIABLE. The function returns three values:

- A category, a keyword among the following:

  + :VAR if TERM is a logical variable.

  + :ATOMIC if TERM is a ground value that cannot be
    decomposed into sub-terms.

  + :GROUND if TERM is a ground value that is composed
    of ground sub-terms.

  + :COMPOUND if TERM is non-ground non-atomic value composed of
    variables, compound or ground sub-terms.

- A hash-table (test EQ) mapping each variable present in TERM to its
  number of occurrences, or NIL if IGNORE-VARIABLES is T.  This
  hash-table is useful to extract the set of all variables in a term.
  When all variables are present only once, the term is said to be
  linear.

- A hash-table (test EQUALP) mapping each ground term to a count of
  its occurrences in TERM, or NIL if IGNORE-GROUND-TERMS is T. This
  value is used for example by COPY-TERM to share common sub-terms.

When DEBUGP is non-NIL, the category of each visited sub-term is
printed to *TRACE-OUTPUT*.

VISITOR is a visitor function defaulting to COMPUTE-TERM-INFORMATION.
It can be used to recurse into custom data-structures. The supplied
function must accept a TERM mandatory argument and the keyword
argument WALKER.

VISITOR is expected to return a category among :ATOMIC, :GROUND
and :COMPOUND (but not :VAR) by calling WALKER with as many argument
as TERM has sub-terms. For example, a VISITOR function that knows how
to visit cons cell is defined as follows:

    (lambda (term &key walker)
      (funcall walker (car term) (cdr term)))

The WALKER function is a closure built by TERM-INFORMATION that knows
how to combine categories from sub-terms (e.g. a term is ground if all
its sub-terms are ground). It is important NOT to call WALKER on each
separate sub-term, but on all sub-terms at once. It should be rarely
needed for VISTOR to explicitly return a category."
  (let ((ground-subterms (unless ignore-ground-terms
                           (make-hash-table :test #'equalp)))
        (variable-counters (unless ignore-variables
                             (make-hash-table :test #'eq)))
        (*detect-multi-walk* nil))
    (declare (special *detect-multi-walk*))
    (labels ((register-ground-term (kind term)
               (prog1 nil
                 (unless ignore-ground-terms
                   (ecase kind
                     ;; :var :compound are non-ground
                     ;; :atomic is :ground but do not register it
                     ;; (it might be register if part of a larger term)
                     ((:var :atomic :compound))
                     (:ground
                      (incf (gethash term ground-subterms 0)))))))
             (register-variable (variable)
               (prog1 :var
                 (unless ignore-variables
                   (incf (gethash variable variable-counters 0)))))
             (recurse (term)
               (let ((category
                       (typecase term
                         (logical-variable
                          (var-check
                           (if (%logical-variable-bound-p term)
                               (recurse (value term))
                               (register-variable term))))
                         (t
                          (let ((*detect-multi-walk* 0))
                            (declare (special *detect-multi-walk*))
                            (nonvar-check
                             (funcall visitor term :walker #'walker)))))))
                 (when debugp
                   (print `(,category ,term) *trace-output*))
                 category))
             (nonvar-check (category)
               (prog1 category
                 (check-type category (member :atomic :ground :compound))))
             (var-check (category)
               (prog1 category
                 (check-type category (member :var :atomic :ground :compound))))
             (groundp (category)
               (member category '(:atomic :ground)))
             (walker (&rest terms)
               (when (> (incf *detect-multi-walk*) 1)
                 ;; TODO: replace by a predefined condition
                 (warn "Multiple calls to WALKER from VISITOR."))
               (loop
                 for category in (mapcar #'recurse terms)
                 for term in terms
                 for groundp = (groundp category)
                 count t into total
                 count groundp into ground-count
                 when groundp
                   collect (cons category term) into ground-terms
                 finally
                    (return
                      (cond
                        ((= total ground-count) :ground)
                        ((zerop ground-count) :compound)
                        (t (dolist (pair ground-terms :compound)
                             (destructuring-bind (groundp . term) pair
                               (register-ground-term groundp term)))))))))
      (values (recurse term)
              variable-counters
              ground-subterms))))

(defun term-variables (term)
  (hash-table-keys
   (nth-value 1 (term-information term :ignore-ground-terms t))))

(defun %replace-by-copies (variables-counter)
  (maphash (lambda (k v)
             (declare (ignore v))
             (setf (gethash k variables-counter)
                   (copy-variable k)))
           variables-counter)
  variables-counter)

(defgeneric make-copy (term &optional deep)
  (:documentation
   "Copying a term. Specialize for user-defined terms.")
  (:method (any &optional deep)
    (cond
      (deep (typecase any
              ((or number character) any)
              (t (call-next-method))))
      (t any))))

(defun copy-term (term &optional (copy-function #'make-copy))
  "... shares common ground subterms ..."
  (multiple-value-bind (category variables subterms) (term-information term)
    (when (eq category :ground)
      (return-from copy-term term))
    (%replace-by-copies variables)
    (labels ((recurse (term)
               (if (gethash term subterms)
                   term
                   (typecase term
                     (logical-variable
                      (if (%logical-variable-bound-p term)
                          (recurse (value term))
                          (gethash term variables term)))
                     (cons (cons (recurse (car term))
                                 (recurse (cdr term))))
                     (t (if copy-function
                            (funcall copy-function term)
                            (error "Cannot copy")))))))
      (values
       (recurse term)
       (hash-table-alist variables)))))

(defun deep-copy-term (term &optional (copy-function #'make-copy))
  "... deep copy ..."
  (let ((variables (%replace-by-copies
                    (nth-value 1 (term-information
                                  term
                                  :ignore-ground-terms t)))))
    (labels ((recurse (term)
               (typecase term
                 (cons (cons (recurse (car term))
                             (recurse (cdr term))))
                 (logical-variable
                  (or (gethash term variables)
                      (recurse (value term))))
                 (t (if copy-function
                        (funcall copy-function term)
                        (error "Cannot copy"))))))
      (values
       (recurse term)
       ;; an alist is easier to use in a bidirectional way
       (hash-table-alist variables)))))

;;;; COLLAPSIBLE

;; Collapsible variables vs. domains w.r.t. undo?

(defclass collapsible-logical-variable (logical-variable) ()
  (:documentation
   "A logic variable which is adjusted when accessing its VALUE slot
   to remove indirection through intermediate collapsible
   variables. The VALUE slot is either unbound, points to a ground
   value, or to a non-collapsible variable."))

(defmethod value ((variable collapsible-logical-variable))
  (%set-logical-variable-value (call-next-method) variable))

;;;; USER-FRIENDLY BINDING

(defun parse-logical-binding-list (variables)
  "Parse the list of symbols for WITH-LOGICAL-VARIABLES."
  (flet ((parse-fresh (variable)
           (destructuring-bind (symbol &optional name) (ensure-list variable)
             (setf name (or name `(quote ,symbol)))
             (list (gensym) `(var :name ,name) symbol)))
         (parse-rebinding (symbol)
           (check-type symbol symbol)
           (list (copy-symbol symbol) symbol symbol)))
    (multiple-value-bind (fresh rebinding)
        (let ((position (position '&environment variables)))
          (if position
              (values (subseq variables 0 position)
                      (subseq variables (1+ position)))
              variables))
      (nconc (mapcar #'parse-fresh fresh)
             (mapcar #'parse-rebinding rebinding)))))

(defmacro with-logical-variables ((&rest variables) &body body)
  "Bind or rebind symbols from VARIABLES as logic variables.

VARIABLES is a list made of two sections delimited by &ENVIRONMENT:

    (V1 ... VN &ENVIRONMENT S1 ... SM)

The left part (V1 ... VN) contains bindings for FRESH logical
variables. Each V can be a SYMBOL, or a list which destructures
as (SYMBOL &optional NAME), where NAME defaults to SYMBOL, quoted.

The right part (S1 ... SM) contains symbols which refer to EXISTING
symbols in the lexical environment. The macro is used to manipulate
existing variables as logical variables. The &ENVIRONMENT keyword is
optional. A list (V1 ... VN) is equivalent to (V1 ... VN &ENVIRONMENT)
where the right part is thus empty.

Both parts are used to establish symbol macros around BODY. More
precisely, each VAR? symbol expands as (VALUE VAR), where VAR is an
underlying logical variable. If VAR? is taken from the FRESH bindings,
VAR is a new logical variable built with #'VAR. Otherwise, VAR refers
to the enclosing binding. In both cases:

    - if VAR is a value or is bound to a value, VAR? expands to that
      value. Otherwise, VAR? expands to VAR;

    - any (SETF ?VAR VAL) form expands as (SETF (VALUE ?VAR) VAL),
      which is equivalent to (UNIFY ?VAR VAL). This is useful in
      particular when using macros that expands to SETF forms.

Moreover, calls to UNIFY within BODY are treated specially so that that
symbols representing logical variables refer to those variables and
not their values. This brings more context to error handling.

In the example below, note how the resulting list contains
values (here: symbols), and not logical variables anymore. This is
because variables referenced by ?a and ?b are instantiated to ground
terms, and thus ?a and ?b evaluate as those values directly. In order
to obtain the same behaviour outside of this macro, it is necessary to
use (VALUE ?A) and (VALUE ?B) explicitly.

    (with-logical-variables (?a ?b)
      (unify `(x ,?a) `(,?b y))
      (assert (equal (list ?a ?b) '(y x))))

Same thing with numbers, using SETF for multiple unifications:

    (with-logical-variables (?a ?b)
      (setf ?a ?b ?b 3)
      (assert (equal '(3 3) (list ?a ?b))))

Chained unification with SHIFTF:

    (with-logical-variables (?a ?b ?c ?d)
      (shiftf ?a ?b ?c ?d 0)
      (assert (every #'zerop (list ?a ?b ?c ?d))))

Notice that WITH-LOGICAL-VARIABLES is only used for easing the creation
and use of logical variables, but is not required to manipulate them:

    ;; works only with constants (this is sufficient for tests)
    (defun infer-type (?type expr)
      (unify ?type (etypecase expr
                     (number :number)
                     (string :string))))

The &ENVIRONMENT keyword is useful notably when accepting logical
variables as function parameters. Continuing with the inference system
example, here below ?RESULT, ?LEFT and ?RIGHT are placeholders for the
types of their respective expressions. The names of fresh logical
variables are set explicitly: they contain additional information
about what their variables represent.

    (defun infer-binop (?result op left right)
      (with-logical-variables ((?left `(type-of ',left))
                               (?right `(type-of ',right))
                               &environment ?result)
        (ecase op
          ((+ - * /) (shiftf ?left ?right ?result :number))
          ((< > =)
           (shiftf ?left ?right :number)
           (unify ?result :bool)))
        (infer-type ?left left)
        (infer-type ?right right)))
"
  (loop
    for (internal value symbol) in (parse-logical-binding-list variables)
    collect `(,internal ,value) into let-bindings
    collect `(,symbol (value ,internal)) into sym-bindings
    collect `(,symbol ,internal) into real-bindings
    finally (return
              (with-gensyms (a b)
                `(let ,let-bindings
                   (symbol-macrolet ,sym-bindings
                     (macrolet ((unify (,a ,b)
                                  `(symbol-macrolet ,',real-bindings
                                     (unify% ,,a ,,b))))
                       ,@body)))))))

;; just to avoid infinite macro expansions
(defun unify% (a b) (unify a b))
