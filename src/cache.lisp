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

(defmethod print-object ((obj cache) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (bt:with-lock-held ((slot-value obj 'lock))
      (princ "count " stream)
      (princ (hash-table-count (slot-value obj 'hash)) stream)
      (princ " size " stream)
      (princ (slot-value obj 'size) stream)
      (princ "/" stream)
      (prin1 (cache-max-size obj) stream))))

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

(defun prepare-cleanup (entry hash)
  (cond ((eq *cleanup-list* t)
	 (remhash (entry-key entry) hash))
	((zerop (entry-busy entry))
	 (remhash (entry-key entry) hash)
	 (push (slot-value entry 'data) *cleanup-list*))
	((< (entry-busy entry) 0)
	 (error "Internal error: double prepare-cleanup for ~s" entry))
	(t
	 (setf (entry-busy entry) (- (entry-busy entry))))))

(defun ensure-cache-size (cache)
  (with-slots (policy hash max-size size) cache
    (loop while (> size max-size)
	  for old = (evict-entry policy (- max-size size))
	  while old
	  do (progn
	       (decf size (slot-value old 'size))
	       (prepare-cleanup old hash)))))

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

(defmethod cache-fetch ((cache cache) key &key only-if-cached force-fetch)
  "Fetch an item for the given key.
If the item is not currently in the cache, or has expired, it is fetched from the provider and stored in the cache.
If force-fetch is specified, a new value is fetched from the provider even if it already exists in the cache.
If a cleanup function is defined for the cache, remember to call cache-release with the second value returned by cache-fetch!"
  (with-slots (lock hash policy provider) cache
    (with-collected-cleanups (cache)
      (multiple-value-bind (hit data entry)
	  (bt:with-lock-held (lock)
	    (when force-fetch
	      (let ((entry (gethash key hash)))
		(when entry
		  (prepare-cleanup entry hash)
		  (decf (slot-value cache 'size) (slot-value entry 'size))
		  (when policy
		    (entry-removed policy entry)))))
	    (flet ((miss ()
		     (let ((entry (make-instance 'cache-entry :key key :pending (bt:make-condition-variable))))
		       (setf (gethash key hash) entry)
		       (values nil entry))))
	      (loop
		 (let ((entry (gethash key hash)))
		   (cond ((and (null entry)
			       only-if-cached)
			  ;; cache miss, and no waiting
			  (return (values t nil nil)))

			 ((null entry)
			  ;; cache miss - initialize fetch from source
			  (return (miss)))

			 ((and (slot-boundp entry 'pending)
			       only-if-cached)
			  ;; cache hit - but data not yet ready, and no waiting
			  (return (values t nil nil)))

			 ((slot-boundp entry 'pending)
			  ;; cache hit - but data not yet ready
			  (let ((pending (slot-value entry 'pending)))
			    (bt:condition-wait pending lock)
			    ;; note: the pending slot is no longer bound after the wait
			    (bt:condition-notify pending)
			    ;; data now available
			    (when (eq (gethash key hash) entry)
			      ;; ... and not immediately cleaned up
			      (if (cache-cleanup cache)
				  (progn
				    (if (>= (entry-busy entry) 0)
					(incf (entry-busy entry))
					(decf (entry-busy entry)))
				    (return (values t (slot-value entry 'data) entry)))
				  (return (values t (slot-value entry 'data)))))))

			 ((and entry policy
			       (or (and (slot-boundp entry 'expiry)
					(<= (slot-value entry 'expiry)
					    (get-universal-time)))
				   (and (>= (entry-busy entry) 0)
					(not (access-entry policy entry)))))
			  ;; cached data has expired or been invalidated
			  (remhash key hash)
			  (prepare-cleanup entry hash)
			  (decf (slot-value cache 'size) (slot-value entry 'size))
			  (entry-removed policy entry)
			  (if only-if-cached
			      (return (values t nil nil)) ; no waiting
			      (return (miss))))

			 ((cache-cleanup cache)
			  (if (>= (entry-busy entry) 0)
			      (incf (entry-busy entry))
			      (decf (entry-busy entry)))
			  (return (values t (slot-value entry 'data) entry)))

			 (t
			  (return (values t (slot-value entry 'data) nil))))))))
	(if hit
	    (values data entry)
	    (multiple-value-bind (content size)
		(handler-case (funcall provider key)
		  (error (e)
		    (bt:with-lock-held (lock)
		      (remhash key hash)
		      (bt:condition-notify (slot-value data 'pending))
		      (slot-makunbound data 'pending))
		    (error e)))
	      (with-collected-cleanups (cache)
		(unless (typep size 'real)
		  (setf size (if content 1 0))
		  (warn "Cache provider did not return a proper size for the data - assuming size of ~d" size))
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
		    (entry-added policy data))
		  (if (cache-cleanup cache)
		      (progn
			(incf (entry-busy data))
			(values content data))
		      (values content nil))))))))))

(defmethod cache-release ((cache cache) entry)
  "Releases a reference for an item fetched earlier.
An item fetched from the cache with cache-fetch will not be cleaned up before it is released."
  (when entry
    (with-slots (lock hash cleanup) cache
      (let ((to-clean (bt:with-lock-held (lock)
			(let ((busy (entry-busy entry)))
			  (cond ((zerop busy)
				 (error "Double release for item with the key ~a" (entry-key entry)))
				((> busy 0)
				 (decf (entry-busy entry))
				 nil)
				(t
				 (when (zerop (incf (entry-busy entry)))
				   (when (eq (gethash (entry-key entry) hash) entry)
				     (remhash (entry-key entry) hash))
				   (slot-value entry 'data))))))))
	(when (and cleanup to-clean)
	  (funcall cleanup to-clean)))))
  nil)

(defmacro with-cache-fetch (var (cache key &key only-if-cached) &body body)
  "Combines a cache-fetch and cache-release in a form."
  (let ((c-var (gensym))
	(tag (gensym)))
    `(let ((,c-var ,cache))
       (multiple-value-bind (,var ,tag)
	   (cache-fetch ,c-var ,key ,@(and only-if-cached '(:only-if-cached t)))
	 (unwind-protect
	      (progn ,@body)
	   (cache-release ,c-var ,tag))))))

(defmethod cache-remove ((cache cache) key)
  "Remove the item with the specified key from the cache."
  (with-slots (lock hash policy size) cache
    (with-collected-cleanups (cache)
      (bt:with-lock-held (lock)
	(let ((entry (gethash key hash)))
	  (when entry
	    (prepare-cleanup entry hash)
	    (decf size (slot-value entry 'size))
	    (when policy
	      (entry-removed policy entry))
	    t))))))

(defmethod cache-flush ((cache cache))
  "Flush the cache, removing all items currently stored in it. If a cleanup function is defined for the cache, it is called for every item."
  (with-slots (lock hash policy size cleanup) cache
    (with-collected-cleanups (cache)
      (bt:with-lock-held (lock)
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (prepare-cleanup v hash)
		     (entry-removed policy v))
		 hash)
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
		       (when (>= (entry-busy v) 0)
			 (unless (gethash v seen)
			   (error "Cache entry missing from policy: ~s" v))
			 (incf total (entry-size v))))
		   hash)
	  (unless (= total size)
	    (error "Cache size mismatch: cache reports ~a, sum of entries is ~a" size total))))))
  t)

#+5am
(5am:test cache-basics-1
  "Ensure that correct items are returned and flush clears the cache."
  (with-testing-cache (cache 200 :policy :fifo :without-cleanup t)
    (let ((items (loop for i from 1 to 15
		       for item = (cache-fetch cache i)
		       do (5am:is (= (first item) i))
		       collect item)))
      (cache-sanity-check cache)
      (dolist (item items)
	(5am:is (eq item (cache-fetch cache (first item)))))
      (cache-flush cache)
      (cache-sanity-check cache)
      (dolist (item items)
	(let ((new (cache-fetch cache (first item))))
	  (5am:is (not (eq item new)))
	  (5am:is (> (second new) 15))))
      (cache-sanity-check cache))))

#+5am
(5am:test cache-basics-2
  "Ensure that correct items are returned and everything is cleaned up on a flush."
  (with-testing-cache (cache 200 :policy :fifo)
    (let ((items (loop for i from 1 to 15
		       for item = (fetch-and-release cache i)
		       do (5am:is (= (first item) i))
		       collect item)))
      (cache-sanity-check cache)
      (dolist (item items)
	(5am:is (eq item (fetch-and-release cache (first item)))))
      (cache-flush cache)
      (cache-sanity-check cache)
      (dolist (item items)
	(5am:is (cleaned-up-p item)))
      (dolist (item items)
	(with-cache-fetch new (cache (first item))
	  (5am:is (not (eq item new)))
	  (5am:is (not (cleaned-up-p new)))
	  (5am:is (> (second new) 15))))
      (cache-sanity-check cache))))

#+5am
(5am:test cache-release
  "Ensure that items are cleaned up only after cache-release."
  (with-testing-cache (cache 100 :policy :fifo)
    (multiple-value-bind (a a-tag)
	(cache-fetch cache 50)
      (let ((b (fetch-and-release cache 49))
	    (c (fetch-and-release cache 30)))
	(5am:is (eq a (fetch-and-release cache 50)))
	(5am:is (not (cleaned-up-p a)))
	(5am:is (eq b (fetch-and-release cache 49)))
	(5am:is (not (cleaned-up-p b)))
	(5am:is (eq c (fetch-and-release cache 30)))
	(5am:is (not (cleaned-up-p c)))
	(cache-release cache a-tag)
	(5am:signals error (cache-release cache a-tag))
	(5am:is (cleaned-up-p a))
	(5am:is (not (eq a (fetch-and-release cache 50))))))))

#+5am
(5am:test cache-size-limit
  "Ensure that cache handles its size limit properly."
  (with-testing-cache (cache 100 :policy :fifo)
    (let ((a (fetch-and-release cache 50))
	  (b (fetch-and-release cache 49))
	  (c (fetch-and-release cache 30)))
      (cache-sanity-check cache)
      (5am:is (cleaned-up-p a))
      (5am:is (not (cleaned-up-p b)))
      (5am:is (not (cleaned-up-p c)))
      (let ((d (fetch-and-release cache 101)))
	(cache-sanity-check cache)
	(5am:is (cleaned-up-p b))
	(5am:is (cleaned-up-p c))
	(5am:is (not (cleaned-up-p d)))
	(let ((e (fetch-and-release cache 1)))
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
				      (bt:make-thread #'(lambda ()
							  (multiple-value-bind (item tag)
							      (cache-fetch cache key)
							    (cache-release cache tag)
							    item))))))
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

#+5am
(5am:test cache-expiry
  "Check that time-based automatic expiry works."
  (with-testing-cache (cache 100 :policy :fifo :lifetime 0)
    ;; zero-second lifetime means that the sme entry should never be returned twice
    (let ((a1 (fetch-and-release cache 20))
	  (b1 (fetch-and-release cache 29))
	  (a2 (fetch-and-release cache 20))
	  (b2 (fetch-and-release cache 29)))
      (5am:is (not (eq a1 a2)))
      (5am:is (not (eq b1 b2)))
      (5am:is (cleaned-up-p a1))
      (5am:is (not (cleaned-up-p a2)))
      (multiple-value-bind (a3 tag-a3)
	  (cache-fetch cache 20)
	(multiple-value-bind (a4 tag-a4)
	    (cache-fetch cache 20)
	  (5am:is (not (eq a2 a3)))
	  (5am:is (not (eq a3 a4)))
	  (5am:is (not (cleaned-up-p a3)))
	  (5am:is (not (cleaned-up-p a4)))
	  (cache-release cache tag-a3)
	  (cache-release cache tag-a4)
	  (5am:is (cleaned-up-p a3))
	  (cache-sanity-check cache))))))
