(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-lock (&key (name "lock"))
  #+allegro
  (mp:make-process-lock :name name)
  #+lispworks
  (mp:make-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+openmcl
  (ccl:make-lock name)
  #-(or allegro lispworks sbcl openmcl)
  (not-implemented 'make-lock))


(defmacro with-lock ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock) ,@body)
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,lock) ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or allegro lispworks sbcl openmcl)
  (not-implemented 'with-lock))

(defmacro with-recursive-lock ((lock) &body body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,lock) ,@body)
  #-(or sbcl)
  (not-implemented 'with-recursive-lock))

(defun process-lock (lock)
  #+lispworks
  (mp:process-lock lock)
  #+sbcl
  (sb-thread:get-mutex lock)
  #-(or sbcl lispworks)
  (not-implemented 'process-lock))


(defun process-unlock (lock)
  #+lispworks
  (mp:process-unlock lock)
  #+sbcl
  (sb-thread:release-mutex lock)
  #-(or sbcl lispworks)
  (not-implemented 'process-unlock))
