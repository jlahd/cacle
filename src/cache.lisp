(in-package #:cacle)

#+5am
(5am:in-suite cacle-tests)

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
  "Create a new cache with the specified maximum size, provider function, and options."
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
  "Returns the current size of the cache."
  (bt:with-lock-held ((slot-value cache 'lock))
    (slot-value cache 'size)))

(defmethod cache-count ((cache cache))
  "Returns the current count of items in the cache."
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
  "Fetch an item for the given key. If the item is not currently in the cache, or has expired, it is fetched from the provider and stored in the cache."
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
  "Remove the item with the specified key from the cache."
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
  "Flush the cache, removing all items currently stored in it. If a cleanup function is defined for the cache, it is called for every item."
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

(defmethod cache-sanity-check ((cache cache))
  (with-slots (lock hash policy size) cache
    (bt:with-lock-held (lock)
      (let ((seen (make-hash-table :test 'eq)))
	(dolist (i (list-entries policy))
	  (let ((v (gethash (entry-key i) hash)))
	    (unless v
	      (error "Cachen entry missing from hashtable: ~s" i))
	    (unless (eq i v)
	      (error "Cache entry mismatch: ~s in hashtable, ~s in policy" v i)))
	  (setf (gethash i seen) t))
	(let ((total 0))
	  (maphash #'(lambda (k v)
		       (declare (ignore k))
		       (unless (gethash v seen)
			 (error "Cache entry missing from policy: ~s" v))
		       (incf total (entry-size v)))
		   hash)
	  (unless (= total size)
	    (error "Cache size mismatch: cache reports ~a, sum of entries is ~a" size total))))))
  t)

#+5am
(5am:test cache-basics
  "Ensure that correct items are returned and everything is cleaned up on a flush."
  (with-testing-cache (cache 200 :policy :fifo)
    (let ((items (loop for i from 1 to 15
		       for item = (cache-fetch cache i)
		       do (5am:is (= (first item) i)))))
      (cache-sanity-check cache)
      (dolist (item items)
	(5am:is (eq item (cache-fetch cache (first item)))))
      (cache-flush cache)
      (cache-sanity-check cache)
      (dolist (item items)
	(5am:is (cleaned-up-p item)))
      (dolist (item items)
	(let ((new (cache-fetch cache (first item))))
	  (5am:is (not (eq item new)))
	  (5am:is (not (cleaned-up-p new)))
	  (5am:is (> (second new) 15))))
      (cache-sanity-check cache))))

#+5am
(5am:test cache-size-limit
  "Ensure that cache handles its size limit properly."
  (with-testing-cache (cache 100 :policy :fifo)
    (let ((a (cache-fetch cache 50))
	  (b (cache-fetch cache 49))
	  (c (cache-fetch cache 30)))
      (cache-sanity-check cache)
      (5am:is (cleaned-up-p a))
      (5am:is (not (cleaned-up-p b)))
      (5am:is (not (cleaned-up-p c)))
      (let ((d (cache-fetch cache 101)))
	(cache-sanity-check cache)
	(5am:is (cleaned-up-p b))
	(5am:is (cleaned-up-p c))
	(5am:is (not (cleaned-up-p d)))
	(let ((e (cache-fetch cache 1)))
	  (cache-sanity-check cache)
	  (5am:is (cleaned-up-p d))
	  (5am:is (not (cleaned-up-p e))))))))

#+5am
(5am:test cache-threading
  "Ensure that simulatenous requests from multiple threads are handled correctly."
  (let ((object 0)
	(lock (bt:make-lock "mutex for test cache")))
    (flet ((provider (arg)
	     (sleep 1)
	     (bt:with-lock-held (lock)
	       (values (list arg (incf object)) arg)))
	   (cleanup (arg)
	     (5am:is (listp arg))
	     (5am:is (= 2 (length arg)))
	     (setf (second arg) :cleaned-up)))
      (let* ((cache (make-cache 100 #'provider :policy :fifo :cleanup #'cleanup))
	     (threads (loop for i below 32
			    collect (let ((key (1+ (mod i 8))))
				      (bt:make-thread #'(lambda () (cache-fetch cache key))))))
	     (a (mapcar #'bt:join-thread threads))
	     (b (nthcdr 8 a))
	     (c (nthcdr 8 b))
	     (d (nthcdr 8 c)))
	(loop for ai in a
	      for bi in b
	      for ci in c
	      for di in d
	      for key from 1
	      do (5am:is (listp ai))
	      do (5am:is (= 2 (length ai)))
	      do (5am:is (= key (first ai)))
	      do (5am:is (integerp (second ai)))
	      do (5am:is (eq ai bi))
	      do (5am:is (eq ai ci))
	      do (5am:is (eq ai di)))
	(cache-sanity-check cache)))))
