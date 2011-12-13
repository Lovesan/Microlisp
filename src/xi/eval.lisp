(in-package #:microlisp)

(enable-xi-syntax)

(define-condition lisp-runtime-error (error)
  ((message :initarg :message
            :reader lisp-runtime-error-message))
  (:default-initargs :message nil)
  (:report (lambda (condition stream)
             (format stream "Lisp runtime error~:[.~; ~:*~a~]"
                     (lisp-runtime-error-message condition))
             condition)))

(defun lisp-runtime-error (message &rest args)
  (error 'lisp-runtime-error
         :message (apply #'format nil message args)))

(defvar *dynamic-vars* '())
(defvar *catch-tags* '())
(defvar +global-var-info+ (if (boundp '+global-var-info+)
                            (symbol-value '+global-var-info+)
                            (make-hash-table :test #'eq)))

;; We really must implement this as a target lisp class.
(defstruct (env (:constructor %make-env))
  vars
  tags
  blocks
  cenv) ;; cenv is a `compiler environment'.
        ;; Macros and constants are evaluated in this environment.
       ; ; It holds macro/constant variable bindings.

(defun make-env (&key parent vars tags blocks)
  (flet ((ct-vars (vars)
           (remove-if
            (lambda (var &aux (kind (cadr var)))
              (not (or (eq kind :macro)
                       (eq kind :constant))))
            vars)))
    (if (null parent)
      (%make-env :vars vars
                 :tags tags
                 :blocks blocks
                 :cenv (%make-env :vars (ct-vars vars)))
      (let ((parent-vars (env-vars parent)))
        (%make-env :vars (append vars parent-vars)
                   :tags (append tags (env-tags parent))
                   :blocks (append blocks (env-blocks parent))
                   :cenv (%make-env
                          :vars (append (ct-vars vars)
                                        (env-vars (env-cenv parent)))))))))

(defun env-var (env symbol)
  (find symbol (env-vars env)
        :key #'car :test #'eq))

(defun env-tag (env symbol)
  (find symbol (env-tags env)
        :key #'car :test #'eq))

(defun env-block (env symbol)
  (find symbol (env-blocks env)
        :key #'car :test #'eq))

(defun error-unbound-variable (symbol)
  (eval-call (sym-value [signal-error])
             (list (sym-value [unbound-variable]) symbol)))

(defun error-undeclared-variable (symbol)
  (eval-call (sym-value [signal-error])
             (list (sym-value [undeclared-variable]) symbol)))

(defun error-constant-modification (symbol)
  (eval-call (sym-value [signal-error])
             (list (sym-value [constant-modification]) symbol)))

(defun error-arg-count (args)
  (eval-call (sym-value [signal-error])
             (list (sym-value [invalid-argument-count]) args)))

(defun error-not-symbol (datum)
  (eval-call (sym-value [signal-error])
             (list (sym-value [class-error])
                   datum
                   (sym-value [symbol]))))

(defun error-not-list (datum)
  (eval-call (sym-value [signal-error])
             (list (sym-value [class-error])
                   datum
                   (sym-value [list]))))

(defun error-not-function (datum)
  (eval-call (sym-value [signal-error])
             (list (sym-value [class-error])
                   datum
                   (sym-value [function]))))

(defun error-not-callable (datum)
  (eval-call (sym-value [signal-error])
             (list (sym-value [instance-not-callable])
                   datum)))

(defun error-not-class-of (datum class)
  (eval-call (sym-value [signal-error])
             (list (sym-value [class-error])
                   datum
                   class)))

(defun get-symbol-value (symbol)
  (let ((dynvar (find symbol *dynamic-vars*
                      :key #'car)))
    (if (not dynvar)
      (case (gethash symbol +global-var-info+)
        ((:static :constant :macro) (sym-value symbol))
        (T (if (eq (sym-value symbol) :unbound)
             (error-unbound-variable symbol)
             (sym-value symbol))))
      (if (eq :unbound (cdr dynvar))
        (error-unbound-variable symbol)
        (cdr dynvar)))))

(defun set-symbol-value (new-value symbol)
  (let ((dynvar (find symbol *dynamic-vars*
                      :key #'car)))
    (if (not dynvar)
      (case (gethash symbol +global-var-info+)
        (:constant (error-constant-modification symbol))
        (T (setf (sym-value symbol) new-value)))
      (setf (cdr dynvar) new-value))))

(defun eval-ref (symbol env)
  (let ((var (env-var env symbol)))
    (if (null var)
      (case (gethash symbol +global-var-info+)
        (:dynamic (get-symbol-value symbol))
        ((:static :constant) (sym-value symbol))
        (:macro (eval-form (sym-value symbol) env))
        (T (error-undeclared-variable symbol)))
      (ecase (second var)
        (:dynamic (get-symbol-value symbol))   
        (:macro (eval-form (cddr symbol) env))
        ((:static :constant) (cddr var))))))

(defun %eval-setq (form env)
  (if (= (length form) 3)
    (if (symbol-object-p (second form))
      (let* ((symbol (second form))
             (value (third form))
             (var (env-var env symbol)))
        (if (null var)
          (case (gethash symbol +global-var-info+)
            (:dynamic (set-symbol-value symbol (eval-form value env)))
            (:constant (error-constant-modification symbol))
            (:macro (eval-form `([setf] ,symbol ,value) env))
            (T (setf (sym-value symbol) (eval-form value env))))
          (ecase (second var)
            (:dynamic (set-symbol-value symbol (eval-form value env)))
            (:static (setf (cddr var) (eval-form value env)))
            (:constant (error-constant-modification symbol))
            (:macro (eval-form `([setf] ,symbol ,value) env)))))
      (error-not-symbol (second form)))
    (error-arg-count (rest form))))

(defun eval-macroexpand (expander form lexenv)
  (declare (ignore lexenv))
  ;; NIL because our env structure is not a lisp instance(yet).
  (eval-call expander (list form nil)))

;; Microlisp set of special operators is very similiar to that
;;  of Common Lisp, so in case of XI we reuse existing CL control structures
;;  and specifically, an implementation of NLX an dynamic environments.
;; Upcoming Microlisp runtime and compiler should implement all
;;   this stuff from scratch.

(defun eval-cons (form env)
  (case (car form)
    ([lambda] (eval-lambda form env))
    ([quote] (eval-quote form env))
    ([setq] (eval-setq form env))
    ([if] (eval-if form env))
    (([toplevel-expansion-too]
      [toplevel-expansion-only]
      [body])
     (eval-body form env))
    ([body1] (eval-body1 form env))
    ([let] (eval-let form env))
    ([let*] (eval-let* form env))
    ([letr] (eval-letr form env))
    ([bind] (eval-bind form env))
    ([catch] (eval-catch form env))
    ([throw] (eval-throw form env))
    ([block] (eval-block form env))
    ([return-from] (eval-return-from env))
    ([tagbody] (eval-tagbody form env))
    ([go] (eval-go form env))
    ([unwind-protect] (eval-unwind-protect form env))
    ([mvcall] (eval-mvcall form env))
    ([the] (eval-the form env))
    (T (let ((f (car form)))
         (if (symbol-object-p f)
           (let ((var (env-var f)))
             (if (null var)
               (case (gethash f +global-var-info+)
                 ((:static :constant)
                  (eval-call (sym-value f) (eval-args (cdr form) env)))
                 (:dynamic
                  (eval-call (get-symbol-value f) (eval-args (cdr form) env)))
                 (:macro (eval-macroexpand (sym-value f) form env))
                 (T (error-undeclared-variable f)))
               (ecase (second var)
                 (:dynamic
                  (eval-call (get-symbol-value f) (eval-args (cdr form) env)))
                 ((:static :constant)
                  (eval-call (cddr var) (eval-args (cdr form) env)))
                 (:macro (eval-macroexpand (cddr var) form env)))))
           (eval-call (eval-form (car form) env)
                      (eval-args (cdr form) env)))))))

(defun eval-args (args env)
  (cond ((null args) '())
        ((atom args) (let ((form (eval-form args env)))
                       (if (listp form)
                         form
                         (error-not-list form env))))
        (T (cons (eval-form (car args) env)
                 (eval-args (cdr args) env)))))

(defun eval-call (f args)
#| TBD
  (let* ((f (typecase f
              (function-object f)
              (instance-object (or (inst-function f)
                                   (error-not-callable f)))
              (T (error-not-function f))))
         (required '())
         (rest nil)
         (*dynamic-vars* *dynamic-vars*))
    (loop :for (p . rp) :on (fun-args f)
          :for (a . ra) :on args
          :if (not (is-p a (cddr p)))
          :do (error-not-class-of a (cddr p))
          :else if (eq :dynamic (cadr p))
          :do (push (cons (car a) p) *dynamic-vars*)
          :else :collect (list* (car p) (cadr p) a) :into req
          :finally )
  )
|#
  )

(defun eval-form (form env)
  (typecase form
    (null nil)
    (symbol-object (eval-ref form env))
    (cons (eval-cons form env))
    ((or character array number) form)
    (T (lisp-runtime-error
        "Invalid lisp object spotted: ~s" form))))

(disable-xi-syntax)
