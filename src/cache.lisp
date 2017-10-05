(in-package #:cacle)

(defclass cache ()
  ((max-size :initarg :max-size :reader cache-max-size)
   (size :initform 0)
   (lock :initform (bt:make-lock "cache"))
   (hash)
   (provider :initarg :provider :reader cache-provider)
   (cleanup :initarg :cleanup :initform nil :reader cache-cleanup)
   (lifetime :initarg :lifetime :initform nil :reader cache-lifetime)
   (policy :initform :fifo :initarg :policy :reader cache-policy)))

(defmethod initialize-instance ((cache cache) &rest initargs &key policy provider (hash-test 'eql) &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method)
  (unless provider
    (error ":provider must be defined"))
  (setf (slot-value cache 'hash) (make-hash-table :test hash-test))
  (cond ((and policy (null (cache-max-size cache)))
	 (error "Policy defined, but no maximum size"))
	((null policy)
	 (unless (null (cache-max-size cache))
	   (error "Maximum size defined, but policy missing")))
	((typep policy 'replacement-policy)
	 (setf (slot-value cache 'policy) policy))
	(t
	 (error "Invalid policy ~s" policy))))

(defun make-cache (max-size provider &key (test 'eql) (policy :fifo) lifetime cleanup)
  (when (or (keywordp policy)
	    (and (listp policy)
		 (keywordp (first policy))))
    (let ((base-type (if (keywordp policy) policy (first policy)))
	  (args (if (keywordp policy) nil (rest policy))))
      (setf policy (apply #'make-instance
			  (ecase base-type
			    (:bélády (error "Clairvoyance hardware not installed"))
			    (:fifo 'fifo-replacement-policy)
			    (:lifo 'lifo-replacement-policy)
			    (:lru 'lru-replacement-policy)
			    (:mru 'mru-replacement-policy)
			    (:random 'random-replacement-policy)
			    (:lfu 'lfu-replacement-policy)
			    (:lfuda 'lfuda-replacement-policy))
			  args))))
  (make-instance 'cache
		 :hash-test test
		 :max-size max-size
		 :provider provider
		 :policy policy
		 :lifetime lifetime
		 :cleanup cleanup))

(defvar *cleanup-list*)
(defmacro with-collected-cleanups ((cache) &body body)
  (let ((i (gensym))
	(fn (gensym)))
    `(let* ((,fn (bt:with-lock-held ((slot-value ,cache 'lock))
		   (slot-value ,cache 'cleanup)))
	    (*cleanup-list* (null ,fn)))
       (unwind-protect
	    (progn ,@body)
	 (when ,fn
	   (dolist (,i *cleanup-list*)
	     (funcall ,fn ,i)))))))

(defun collect-cleanup (data)
  (unless (eq *cleanup-list* t)
    (push data *cleanup-list*)))

(defun ensure-cache-size (cache)
  (with-slots (policy hash max-size size) cache
    (loop while (> size max-size)
	  for old = (evict-entry policy (- max-size size))
	  while old
	  do (progn
	       (decf size (slot-value old 'size))
	       (remhash (slot-value old 'key) hash)
	       (collect-cleanup (slot-value old 'data))))))

(defmethod cache-size ((cache cache))
  (bt:with-lock-held ((slot-value cache 'lock))
    (slot-value cache 'size)))

(defmethod cache-count ((cache cache))
  (bt:with-lock-held ((slot-value cache 'lock))
    (hash-table-count (slot-value cache 'hash))))

(defmethod set-cache-max-size ((cache cache) new-max)
  (with-slots (lock max-size policy) cache
    (with-collected-cleanups (cache)
      (bt:with-lock-held (lock)
	(setf max-size new-max)
	(when policy
	  (ensure-cache-size cache))))))

(defsetf cache-max-size set-cache-max-size)

(defmethod set-cache-provider ((cache cache) new-provider)
  (with-slots (lock provider) cache
    (bt:with-lock-held (lock)
      (setf provider new-provider))))

(defsetf cache-provider set-cache-provider)

(defmethod set-cache-cleanup ((cache cache) new-cleanup)
  (with-slots (lock cleanup) cache
    (bt:with-lock-held (lock)
      (setf cleanup new-cleanup))))

(defsetf cache-cleanup set-cache-cleanup)

(defmethod set-cache-lifetime ((cache cache) new-lifetime)
  (with-slots (lock lifetime) cache
    (bt:with-lock-held (lock)
      (setf lifetime new-lifetime))))

(defsetf cache-lifetime set-cache-lifetime)

(defmethod cache-fetch ((cache cache) key)
  (with-slots (lock hash policy provider) cache
    (multiple-value-bind (hit data)
	(bt:with-lock-held (lock)
	  (flet ((miss ()
		   (let ((entry (make-instance 'cache-entry :key key :pending (bt:make-condition-variable))))
		     (setf (gethash key hash) entry)
		     (values nil entry))))
	    (let ((entry (gethash key hash)))
	      (cond ((null entry)
		     ;; cache miss - initialize fetch from source
		     (miss))
		    ((slot-boundp entry 'pending)
		     ;; cache hit - but data not yet ready
		     (let ((pending (slot-value entry 'pending)))
		       (bt:condition-wait pending lock)
		       ;; note: the pending slot is no longer bound after the wait
		       (bt:condition-notify pending)
		       ;; data now available
		       (values t (slot-value entry 'data))))
		    ((and entry policy
			  (or (and (slot-boundp entry 'expiry)
				   (<= (slot-value entry 'expiry)
				       (get-universal-time)))
			      (not (access-entry policy entry))))
		     ;; cached data has expired or been invalidated
		     (remhash key hash)
		     (decf (slot-value cache 'size) (slot-value entry 'size))
		     (entry-removed policy entry)
		     (miss))
		    (t
		     (values t (slot-value entry 'data)))))))
      (if hit
	  data
	  (multiple-value-bind (content size)
	      (funcall provider key)
	    (with-collected-cleanups (cache)
	      (bt:with-lock-held (lock)
		(setf (slot-value data 'data) content
		      (slot-value data 'size) size)
		(with-slots (lifetime) cache
		  (when lifetime
		    (setf (slot-value data 'expiry)
			  (+ (get-universal-time) lifetime))))
		(bt:condition-notify (slot-value data 'pending))
		(slot-makunbound data 'pending)
		(incf (slot-value cache 'size) size)
		(when policy
		  (ensure-cache-size cache)
		  (entry-added policy data))))
	    content)))))

(defmethod cache-remove ((cache cache) key)
  (with-slots (lock hash policy size) cache
    (with-collected-cleanups (cache)
      (bt:with-lock-held (lock)
	(let ((entry (gethash key hash)))
	  (when entry
	    (remhash key hash)
	    (decf size (slot-value entry 'size))
	    (when policy
	      (entry-removed policy entry))
	    (collect-cleanup (slot-value entry 'data))
	    t))))))

(defmethod cache-flush ((cache cache))
  (with-slots (lock hash policy size cleanup) cache
    (with-collected-cleanups (cache)
      (bt:with-lock-held (lock)
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (entry-removed policy v)
		     (collect-cleanup (slot-value v 'data)))
		 hash)
	(clrhash hash)
	(setf size 0)))
    nil))
