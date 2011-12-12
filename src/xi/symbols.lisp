(in-package #:microlisp)

;
; We need to store symbols somewhere untils
;   it comes to namespace and assembly definitions.
;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *xi-symbols* (make-hash-table :test #'equal))
  (defun %symbol (name)
    (declare (type string name))
    (let ((str (coerce name '(simple-array character (*)))))
      (or (gethash str *xi-symbols*)
          (setf (gethash str *xi-symbols*)
                (alloc-sym :name str)))))
  (defmethod make-load-form ((object symbol-object) &optional env)
    (declare (ignore env))
    `(%symbol ,(sym-name object))))

;
; syntax sugar for XI symbols
;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *prev-readtables* '())
  (defun %enable-xi-syntax ()
    (push *readtable* *prev-readtables*)
    (setf *readtable* (copy-readtable))
    (set-macro-character
     #\[ (lambda (s c)
           (declare (ignore c))
           (let ((s (coerce
                     (with-output-to-string (out)
                       (loop :for c = (read-char s) :do
                             (when (char= c #\]) (return))
                             (write-char c out)))
                     '(simple-array character (*)))))
             (%symbol s))))
    (set-macro-character
     #\] (lambda (s c)
           (declare (ignore s c))
           (error "Unmatched close bracket."))))
  (defun %disable-xi-syntax ()
    (unless (endp *prev-readtables*)
      (setf *readtable* (pop *prev-readtables*)))))

(defmacro enable-xi-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-xi-syntax)))

(defmacro disable-xi-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-xi-syntax)))
