(in-package :cacle)

#+5am
(5am:def-suite cacle-tests :description "cacle test suite")

#+5am
(5am:in-suite cacle-tests)

#+5am
(defmacro with-testing-cache ((var size &key policy item-size-modulus cleanup-checks) &body body)
  (let ((provider (gensym))
	(cleanup (gensym))
	(object (gensym))
	(lock (gensym))
	(arg (gensym)))
    `(let ((,object 0)
	   (,lock (bt:make-lock "mutex for with-testing-cache")))
       (flet ((,provider (,arg)
		(bt:with-lock-held (,lock)
		  (values (list ,arg (incf ,object)) ,(if item-size-modulus
							  `(mod ,arg ,item-size-modulus)
							  arg))))
	      (,cleanup (,arg)
		,@(when cleanup-checks
		    `((5am:is (listp ,arg))
		      (5am:is (= 2 (length ,arg)))))
		(setf (second ,arg) :cleaned-up))
	      (cleaned-up-p (,arg)
		(cond ((eq (second ,arg) :cleaned-up)
		       t)
		      ((integerp (second ,arg))
		       nil)
		      (t
		       (error "Corrupted cache data ~s" ,arg)))))
	 (let ((,var (make-cache ,size #',provider :policy (or ,policy :fifo) :cleanup #',cleanup)))
	   ,@body)))))

#+5am
(5am:test bélády-replacement-policy
  (5am:signals error (make-cache 100 #'list :policy :bélády)))

#+5am
(5am:test random-single-thread-testing
  (let ((repetitions 100000))
    (dolist (policy '(:fifo :lifo :lru :mru :random :lfu :lfuda))
      (with-testing-cache (cache 1000 :policy policy :cleanup-checks nil)
	(dotimes (i repetitions)
	  (let* ((key (1+ (random 100)))
		 (data (cache-fetch cache key)))
	    (unless (= (first data) key)
	      (5am:fail "attempt to fetch data for key ~a resulted in ~s" key data))))
	(5am:is (> (cache-size cache) 900))
	(5am:is (> (cache-count cache) 10))
	(handler-case
	    (cache-sanity-check cache)
	  (error (e)
	    (error "With policy ~a: ~a" policy e)))))))

#+5am
(5am:test random-multi-thread-testing
  (let ((threads 4)
	(repetitions 25000))
    (dolist (policy '(:fifo :lifo :lru :mru :random :lfu :lfuda))
      (with-testing-cache (cache 1000 :policy policy :cleanup-checks nil)
	(let ((threads (loop for i below threads
			     collect (bt:make-thread
				      #'(lambda ()
					  (dotimes (i repetitions)
					    (let* ((key (1+ (random 100)))
						   (data (cache-fetch cache key)))
					      (unless (and (= (first data) key)
							   (not (cleaned-up-p data)))
						(5am:fail "attempt to fetch data for key ~a resulted in ~s" key data))))
					  t)))))
	  (5am:is (zerop (count-if-not #'identity (mapcar #'bt:join-thread threads)))))
	(5am:is (> (cache-size cache) 900))
	(5am:is (> (cache-count cache) 10))
	(handler-case
	    (cache-sanity-check cache)
	  (error (e)
	    (error "With policy ~a: ~a" policy e)))))))
