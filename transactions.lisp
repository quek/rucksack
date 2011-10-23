;; $Id: transactions.lisp,v 1.15 2009/05/27 14:26:25 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; User API:
;;;  transaction-start
;;;  transaction-commit
;;;  transaction-rollback
;;;  with-transaction
;;;  current-transaction
;;;
;;; Internal API:
;;;  transaction standard-transaction
;;;  transaction-start-1
;;;  

(defgeneric transaction-start-1 (cache rucksack &key &allow-other-keys)
  (:documentation "Creates and returns a new transaction."))

(defgeneric transaction-commit-1 (transaction cache rucksack)
  (:documentation "Save all modified objects to disk."))

(defgeneric transaction-rollback-1 (transaction cache rucksack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transaction ()
  ())

(defclass standard-transaction (transaction)
  ((id :initarg :id :reader transaction-id)
   ;; Dirty objects
   (dirty-objects :initarg :dirty-objects
                  :initform (make-hash-table)
                  :reader dirty-objects
                  :documentation "A hash-table \(from id to object)
containing all objects of which the slot changes have not been written
to disk yet.")
   (dirty-queue :initarg :dirty-queue
                :initform (make-instance 'queue)
                :reader dirty-queue
                :documentation "A queue with the ids of all objects
that have been created or modified since the last commit.  The queue
is in least-recently-dirtied-first order.  During a commit, the
objects are written to disk in the same order \(this is necessary to
guarantee that the garbage collector never sees an id of an object
that doesn't exist on disk yet.")))

(defclass concurrent-transaction (standard-transaction)
  ((objects :initarg :objects
            :initform (make-hash-table)
            :reader objects
            :documentation "A hash-table \(from id to object)
containing the youngest committed version of all objects that are
currently kept in memory but are not dirty.  \('The youngest version'
means the version belonging to the youngest committed transaction.)")))

(defmethod objects ((cache concurrent-cache))
  (objects (current-transaction)))

(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t :identity nil)
    (format stream "#~D with ~D dirty object~:P"
            (transaction-id transaction)
            (hash-table-count (dirty-objects transaction)))))


(defun current-transaction ()
  *transaction*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying objects and checking for conflicts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric transaction-changed-object (transaction object-id)
  (:documentation
   "If the given transaction has modified the object with the given
object id, this function returns the modified object.  Otherwise it
returns nil."))

(defgeneric transaction-older-p (a b)
  (:documentation
   "Returns true iff transaction A is older than transaction B."))

(defgeneric find-conflicting-transaction (object-id cache transaction)
  (:documentation
   "Tries to find an open transaction that has modified the object
with the given object-id and is older than the given transaction.
Returns this conflicting transaction, if there is one.  Otherwise it
returns nil."))

(defmethod transaction-nr-dirty-objects ((transaction standard-transaction))
  (hash-table-count (dirty-objects transaction)))

(defmethod transaction-touch-object ((transaction standard-transaction)
                                     object
                                     object-id)
  (setf (gethash object-id (dirty-objects transaction)) object)
  (queue-add (dirty-queue transaction) object-id))


(defmethod transaction-changed-object ((transaction standard-transaction)
                                       object-id)
  (gethash object-id (dirty-objects transaction)))


(defmethod find-conflicting-transaction
           (object-id
            (cache standard-cache)
            (current-transaction standard-transaction))
  ;; EFFICIENCY: We need to consider all transactions, because the
  ;; transactions are in a hash-table.  If we use a container that's
  ;; ordered by creation time (like a btree), we only need to consider
  ;; transactions that are younger than the given transaction.
  (loop for transaction being the hash-value of (transactions cache)
        thereis (and (not (eql transaction current-transaction))
                     (transaction-older-p transaction current-transaction)
                     (transaction-changed-object transaction object-id)
                     transaction)))


(defmethod transaction-older-p ((a standard-transaction)
                                (b standard-transaction))
  (< (transaction-id a) (transaction-id b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting a new transaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transaction-start (&rest args
                          &key (rucksack (current-rucksack))
                          &allow-other-keys)
  (apply #'transaction-start-1 (rucksack-cache rucksack) rucksack args))


(defmethod transaction-start-1 ((cache standard-cache)
                                (rucksack standard-rucksack)
                                &key &allow-other-keys)
  ;; Create new transaction.
  (let* ((id (incf (highest-transaction-id rucksack)))
         (transaction (make-instance 'standard-transaction :id id)))
    ;; Add to open transactions.
    (open-transaction cache transaction)
    ;; And return the new transaction.
    transaction))

(defmethod transaction-start-1 ((cache standard-cache)
                                (rucksack concurrent-transaction-rucksack)
                                &key &allow-other-keys)
  (with-global-lock (rucksack)
    ;; Create new transaction.
    (let* ((id (incf (highest-transaction-id rucksack)))
           (transaction (make-instance 'concurrent-transaction :id id)))
      ;; Add to open transactions.
      (open-transaction cache transaction)
      ;; And return the new transaction.
      transaction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rucksacks with serial transactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass serial-transaction-rucksack (standard-rucksack)
  ((transaction-lock :initform (make-lock :name "Rucksack transaction lock")
                     :reader rucksack-transaction-lock))
  (:documentation
   "A serial transaction rucksack allows only one active transaction
at a time."))

(defmethod transaction-start-1 :before ((cache standard-cache)
                                        (rucksack serial-transaction-rucksack)
                                        &key &allow-other-keys)
  (process-lock (rucksack-transaction-lock rucksack)))

(defmethod transaction-commit-1 :after ((transaction standard-transaction)
                                        (cache standard-cache)
                                        (rucksack serial-transaction-rucksack))
  (process-unlock (rucksack-transaction-lock rucksack)))

(defmethod transaction-rollback-1 :after ((transaction standard-transaction)
                                          (cache standard-cache)
                                          (rucksack serial-transaction-rucksack))
  (process-unlock (rucksack-transaction-lock rucksack)))

(defmethod transaction-commit-1 :around ((transaction concurrent-transaction)
                                         (cache standard-cache)
                                         (rucksack concurrent-transaction-rucksack))
  (with-global-lock (rucksack)
    (check-confilict transaction cache)
    (call-next-method)))

(defun check-confilict (transaction cache)
  (loop with heap = (heap cache)
        with object-table = (object-table heap)
        for object being the hash-value of (dirty-objects transaction)
        for object-id = (object-id object)
        if (and (not (eql :reserved (object-info object-table object-id)))
                (/= (load-transaction-id heap object-id)
                    (transaction-id object)))
          do (close-transaction cache transaction)
             (rucksack-error 'transaction-conflict
                        :object-id object-id
                        :new-transaction transaction
                        :old-transaction (load-transaction-id heap object-id))))

(defun load-transaction-id (heap object-id)
  (let* ((block (object-heap-position (object-table heap) object-id))
         (buffer (load-block heap block :skip-header t)))
    (multiple-value-bind (id nr-slots schema-id transaction-id previous)
        (load-object-fields buffer object-id)
      (declare (ignore id nr-slots schema-id previous))
      transaction-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Committing a transaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use without-rucksack-gcing to locally set
;;; *collect-garbage-on-commit* to nil in order to supress rucksack
;;; garbage collection on commit
(defmacro without-rucksack-gcing (&body body)
  `(let ((*collect-garbage-on-commit* nil))
     ,@body))

(defun transaction-commit (transaction &key (rucksack (current-rucksack)))
  "Call transaction-commit-1 to do the real work."
  (transaction-commit-1 transaction (rucksack-cache rucksack) rucksack))

(defmethod transaction-commit-1 ((transaction standard-transaction)
                                 (cache standard-cache)
                                 (rucksack standard-rucksack))
  ;; Save all dirty objects to disk.
  (if (zerop (transaction-nr-dirty-objects transaction))
      (close-transaction cache transaction)
    (progn
      ;; 1. Create the commit file
      (create-commit-file transaction cache)
      ;; 2. Commit all dirty objects.
      ;; Q: What if this is interleaved with other commits?
      (let ((queue (dirty-queue transaction))
            (table (dirty-objects transaction))
            (heap (heap cache))
            nr-allocated-octets)
        (with-allocation-counter (heap)
          (loop until (queue-empty-p queue)
                do (let* ((id (queue-remove queue))
                          (object (gethash id table)))
                     (when object
                       ;; If it's not in the dirty-objects table anymore, the
                       ;; object was already saved during this transaction-commit.
                       ;; That's possible, because the queue can contain duplicates.
                       (save-dirty-object object cache transaction id)
                       ;; Remove from hash-table too.
                       (remhash id table))))
          (setq nr-allocated-octets (nr-allocated-octets heap)))
        ;; Check for consistency between hash table and queue.
        (unless (zerop (hash-table-count table))
          (internal-rucksack-error
           "Mismatch between dirty hash-table and queue while committing ~S:
~D objects left in hash-table."
           transaction
           (hash-table-count table)))
        ;; 3. Remove transaction from the cache's open transactions.
        (close-transaction cache transaction)
        ;; 4. Delete the commit file to indicate that everything went fine
        ;; and we don't need to recover from this commit.
        (delete-commit-file transaction cache)
        ;; 5. Let the garbage collector do an amount of work proportional
        ;; to the number of octets that were allocated during the commit.
        (when *collect-garbage-on-commit*
          (collect-some-garbage heap
                                (gc-work-for-size heap nr-allocated-octets)))
        ;; 6. Make sure that all changes are actually on disk before
        ;; we continue.
        (finish-all-output rucksack)))))

(defmethod finish-all-output ((rucksack standard-rucksack))
  (let ((cache (rucksack-cache rucksack)))
    (finish-heap-output (heap cache))
    (finish-heap-output (object-table (heap cache)))
    ;; NOTE: I'm not totally sure that saving the schema table for
    ;; each transaction commit is necessary, but it probably is.  So
    ;; let's play safe for now.  We definitely need to save the roots,
    ;; because the highest transaction-id is part of the roots file.
    (save-roots rucksack)
    (save-schema-table-if-necessary (schema-table cache))))

                                        
;;
;; Commit file
;;

(defun create-commit-file (transaction cache)
  "Write object ids of all dirty objects to the commit file, so
recovery can do its job if this transaction never completes."
  (with-open-file (stream (commit-filename cache)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (serialize (transaction-id transaction) stream)
    (serialize (hash-table-count (dirty-objects transaction)) stream)
    (loop for object-id being the hash-key of (dirty-objects transaction)
          do (serialize object-id stream))))

(defun delete-commit-file (transaction cache)
  (declare (ignore transaction))
  (delete-file (commit-filename cache)))

(defun load-commit-file (cache)
  "Returns two values: a transaction id and a list of object ids
(of objects that may be partially committed)."
  (with-open-file (stream (commit-filename cache)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (let* ((transaction-id (deserialize stream))
           (nr-objects (deserialize stream))
           (objects (loop repeat nr-objects
                          collect (deserialize stream))))
      (values transaction-id objects))))

;;
;; Saving objects
;;

(defmethod save-dirty-object (object
                              (cache standard-cache)
                              (transaction standard-transaction)
                              object-id &key schema)
  (let* ((transaction-id (transaction-id transaction))
         (heap (heap cache))
         (object-table (object-table heap))
         (version-list
           ;; If the object-table entry is not marked :reserved, there
           ;; is an object version list.  Get the start of that list.
           (and (not (eql :reserved (object-info object-table object-id)))
                (object-heap-position object-table object-id))))
    (let ((block (save-object object object-id cache
                              transaction-id version-list
                              :schema schema)))
      ;; This version becomes the start of the version list.
      (setf (object-heap-position object-table object-id) block)))
  object-id)

(defun (setf object-version-list) (old-block young-block heap)
  "Let the (previous pointer of the) object in YOUNG-BLOCK point to
OLD-BLOCK."
  (with-heap-stream (stream heap)
    (file-position stream (+ young-block (block-header-size heap)))
    (serialize-previous-version-pointer old-block stream))
  old-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rolling back
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transaction-rollback (transaction &key (rucksack (current-rucksack)))
  (transaction-rollback-1 transaction
                          (rucksack-cache rucksack)
                          rucksack))

(defmethod transaction-rollback-1 ((transaction standard-transaction)
                                   (cache standard-cache)
                                   (rucksack standard-rucksack))
  (clrhash (dirty-objects transaction))
  (queue-clear (dirty-queue transaction))
  (close-transaction cache transaction))

(defmethod transaction-rollback-1 :around ((transaction concurrent-transaction)
                                           (cache standard-cache)
                                           (rucksack concurrent-transaction-rucksack))
  (with-global-lock (rucksack)
    (call-next-method)))
