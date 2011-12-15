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
(defvar +global-var-info+ (make-hash-table :test #'eq))

;; We really must implement this as a target lisp class.
(defstruct (env (:constructor %make-env))
  vars
  tags
  blocks)

(defun make-env (&key parent vars tags blocks)
  (if (null parent)
    (%make-env :vars vars
               :tags tags
               :blocks blocks)
    (%make-env :vars (append vars (env-vars parent))
               :tags (append tags (env-tags parent))
               :blocks (append blocks (env-blocks parent)))))

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

(defun error-duplicate-doc (doc)
  (eval-call (sym-value [signal-error])
             (list (sym-value [duplicate-docstring])
                   doc)))

(defun error-invalid-decl (decl)
  (eval-call (sym-value [signal-error])
             (list (sym-value [invalid-declaration])
                   decl)))

(defun error-invalid-decl-context (decl)
  (eval-call (sym-value [signal-error])
             (list (sym-value [invalid-declaration-context])
                   decl)))

(defun error-dup-vardecl (decl)
  (eval-call (sym-value [signal-error])
             (list (sym-value [duplicate-variable-declaration])
                   decl)))

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
        (:static (sym-value symbol))
        (T (if (eq :unbound (sym-value symbol))
             (error-unbound-variable symbol)
             (sym-value symbol))))
      (ecase (second var)
        (:dynamic (get-symbol-value symbol))
        (:static (cddr var))))))

(defun %eval-setq (symbol value env)
  (let ((var (env-var env symbol)))
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
        (:macro (eval-form `([setf] ,symbol ,value) env))))))

(defun expand (form env)
  (cond
    ((symbol-object-p form)
     (let ((var (env-var form)))
       (if (null var)
         (case (gethash form +global-var-info+)
           (:macro (expand (sym-value form)))
           (:constant (sym-value form))
           (t form))
         (ecase (second var)
           (:macro (expand (cddr var)))
           (:constant (cddr var))
           (t form)))))
    ((consp form)
     (if (symbol-object-p (car form))
       (let ((var (env-var (car form))))
         (if (null var)
           (case (gethash (car form) +global-var-info+)
             (:macro (expand (eval-call
                              (sym-value (car form))
                              (list form env))))
             (T form))
           (ecase (second var)
             (:macro (expand (eval-call
                              (cddr var)
                              (list form env))))
             (T form))))
       form))
    (T form)))

(defun parse-body (body &key allow-doc allow-locals allow-macros)
  (multiple-value-bind
        (doc decls forms)
      (loop :with doc = nil
            :with decls = '()
            :for form :on body :do
            (cond ((and allow-doc (stringp (car form)))
                   (if doc
                     (error-duplicate-doc (car form))
                     (setf doc (car form))))
                  ((and (consp form) (eq [declare] (caar form)))
                   (setf decls (append decls (cdar form))))
                  (T (return (values doc decls form))))
            :finally (return (values doc decls nil)))
    (let (statics dynamics constants macros names)
      (dolist (d decls)
        (if (consp d)
          (macrolet ((foo (vtype)
                       (let ((v (gensym))
                             (r (gensym)))
                         `(loop :for  (,v . ,r) :on (cdr d)
                                :if (null ,r) :do (return)
                                :else :if (atom ,r)
                                :do (error-not-list d)
                                :else :if (find ,v names)
                                :do (error-dup-vardecl d)
                                :else :do
                                (push ,v names)
                                (push ,v ,vtype)))))
            (case (car d)
              ([dynamic] (foo dynamics))
              ([static] (if allow-locals
                          (foo statics)
                          (error-invalid-decl-context d)))
              ([constant] (if allow-macros
                            (foo constants)
                            (error-invalid-decl-context d)))
              ([macro] (if allow-macros
                         (foo macros)
                         (error-invalid-decl-context d)))))
          (error-invalid-decl d)))
      (values forms doc statics dynamics constants macros))))

;; Microlisp set of special operators is very similiar to that
;;  of Common Lisp, so in case of XI we reuse existing CL control structures
;;  and specifically, an implementation of NLX an dynamic environments.
;; Upcoming Microlisp runtime and compiler should implement all
;;   this stuff from scratch.

(defun eval-quote (form env)
  (declare (ignore env))
  (if (= 2 (length form))
    (second form)
    (error-arg-count (cdr form))))

(defun eval-setq (form env)
  (let ((pairs (rest form)))
    (if (evenp (length pairs))
      (let (last)
        (loop :for (s v) :on pairs :by #'cddr
              :if (symbol-object-p s)
              :do (setf last (%eval-setq s v env))
              :else :do (error-not-symbol s))
        last)
      (error-arg-count pairs))))

(defun eval-if (form env)
  (case (length form)
    ((3 4) (if (eval-form (second form) env)
             (eval-form (third form) env)
             (eval-form (fourth form) env)))
    (T (error-arg-count (rest form)))))

(defun eval-progn (forms env)
  (let (prev)
    (dolist (expr forms)
      (when prev (eval-form prev env))
      (setf prev expr))
    (eval-form prev env)))

(defun eval-body (form env)
  (multiple-value-bind
        (forms doc statics dynamics constants macros)
      (parse-body (rest form))
    (declare (ignore doc statics constants macros))
    (if (endp dynamics)
      (eval-progn forms env)
      (let ((env (make-env :parent env
                           :vars (mapcar (lambda (v)
                                           (cons v :dynamic))
                                         dynamics))))
        (eval-progn forms env)))))

(defun eval-the (form env)
  (if (= 3 (length form))
    ;; I'm too lazy to implement this.
    ;; TODO: perform type checking
    (eval-form (third form) env)
    (error-arg-count (rest form))))

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
    (T (eval-call (eval-form (car form) env)
                  (eval-args (cdr form) env)))))

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
  (let ((form (expand form env)))
    (typecase form
      (null nil)
      (symbol-object (eval-ref form env))
      (cons (eval-cons form env))
      ((or character array number) form)
      (T (lisp-runtime-error
          "Invalid lisp object spotted: ~s" form)))))

(defun eval-form-in-cenv (form env)
  (let ((cenv (make-env
               :vars (loop :for v :in (env-vars env)
                           :for vt = (second v)
                           :when (or (eq vt :constant)
                                     (eq vt :macro))
                           :collect v))))
    (eval-form form cenv)))

(disable-xi-syntax)
