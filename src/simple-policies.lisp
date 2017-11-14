(in-package #:cacle)

#+5am
(5am:in-suite cacle-tests)

;;; Generic base for caching policies that store entries in a linked list

(defclass linked-list-replacement-policy (replacement-policy)
  ((head :initform (make-instance 'linked-cache-entry) :reader linked-list-head)))

(defmethod print-object ((obj linked-list-replacement-policy) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (loop with head = (slot-value obj 'head)
	  for i = (slot-value head 'next) then (slot-value i 'next)
	  until (eq i head)
	  do (format stream " ~a" (slot-value i 'key)))))

(defmethod entry-added :before ((policy linked-list-replacement-policy) (entry cache-entry))
  (change-class entry 'linked-cache-entry))

(defmethod access-entry ((policy linked-list-replacement-policy) (entry linked-cache-entry))
  ;; No time-based validation
  t)

(defmethod entry-added ((policy linked-list-replacement-policy) (entry cache-entry))
  ;; Push to head of the queue
  (link-after entry (slot-value policy 'head)))

(defmethod entry-removed ((policy linked-list-replacement-policy) (entry linked-cache-entry))
  (unlink entry))

#+5am
(defmethod list-entries ((policy linked-list-replacement-policy))
  (loop with head = (slot-value policy 'head)
	for i = (slot-value head 'next) then (slot-value i 'next)
        until (eq i head)
        collect i))

;;; Generic base for caching policies that store entries in an array

(defclass array-replacement-policy (replacement-policy)
  ((data :initform (make-array 16 :adjustable t :fill-pointer 0))
   (unused :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod entry-added :before ((policy array-replacement-policy) (entry cache-entry))
  (change-class entry 'indexed-cache-entry))

(defmethod access-entry ((policy array-replacement-policy) (entry cache-entry))
  t)

(defmethod entry-added ((policy array-replacement-policy) (entry cache-entry))
  (with-slots (data unused) policy
    (if (zerop (length unused))
	(setf (entry-index entry) (vector-push-extend entry data))
	(let ((i (vector-pop unused)))
	  (setf (entry-index entry) i
		(aref data i) entry)))))

(defmethod entry-removed ((policy array-replacement-policy) (entry indexed-cache-entry))
  (with-slots (data unused) policy
    (let ((i (entry-index entry)))
      (vector-push-extend i unused)
      (setf (aref data i) nil
	    (entry-index entry) nil)
      (when (> (length unused) (/ (length data) 4))
	(let ((w 0))
	  (loop for i below (length data)
		for e = (aref data i)
	        when e
		do (if (= w i)
		       (incf w)
		       (setf (entry-index e) w
			     (aref data w) e
			     w (1+ w))))
	  (setf (fill-pointer data) w
		(fill-pointer unused) 0))))))

#+5am
(defmethod list-entries ((policy array-replacement-policy))
  (loop for i across (slot-value policy 'data)
        when i collect i))

;;; FIFO: Always discard the oldest entry

(defclass fifo-replacement-policy (linked-list-replacement-policy)
  ())

(defmethod evict-entry ((policy fifo-replacement-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (head) policy
    (let ((last (slot-value head 'prev)))
      (unless (eq last head)
	(unlink last)
	last))))

#+5am
(5am:test fifo-replacement-policy
  (with-testing-cache (cache 100 :policy :fifo)
    (let* ((a (cache-fetch cache 20))
	   (b (cache-fetch cache 30))
	   (c (cache-fetch cache 40))
	   (d (cache-fetch cache 25)))
      (5am:is (cleaned-up-p a))
      (5am:is (not (cleaned-up-p b)))
      (5am:is (not (cleaned-up-p c)))
      (5am:is (eq d (cache-fetch cache 25)))
      (5am:is (eq c (cache-fetch cache 40)))
      (5am:is (eq b (cache-fetch cache 30)))
      (cache-fetch cache 35)
      (5am:is (cleaned-up-p b))
      (5am:is (not (cleaned-up-p c)))
      (5am:is (not (cleaned-up-p d)))
      (cache-sanity-check cache))))

;;; LIFO: Always discard the latest entry

(defclass lifo-replacement-policy (linked-list-replacement-policy)
  ())

(defmethod evict-entry ((policy lifo-replacement-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (head) policy
    (let ((first (slot-value head 'next)))
      (unless (eq first head)
	(unlink first)
	first))))

#+5am
(5am:test lifo-replacement-policy
  (with-testing-cache (cache 100 :policy :lifo)
    (let* ((a (cache-fetch cache 20))
	   (b (cache-fetch cache 30))
	   (c (cache-fetch cache 40))
	   (d (cache-fetch cache 25)))
      (5am:is (cleaned-up-p c))
      (5am:is (not (cleaned-up-p b)))
      (5am:is (not (cleaned-up-p a)))
      (5am:is (eq a (cache-fetch cache 20)))
      (5am:is (eq b (cache-fetch cache 30)))
      (5am:is (eq d (cache-fetch cache 25)))
      (cache-fetch cache 35)
      (5am:is (cleaned-up-p d))
      (cache-sanity-check cache))))

;;; LRU: Discard the Least Recently Used entry

(defclass lru-replacement-policy (fifo-replacement-policy)
  ())

(defmethod access-entry ((policy lru-replacement-policy) (entry cache-entry))
  (unlink entry)
  (link-after entry (slot-value policy 'head))
  t)

#+5am
(5am:test lru-replacement-policy
  (with-testing-cache (cache 100 :policy :lru)
    (let* ((a (cache-fetch cache 20))
	   (b (cache-fetch cache 30))
	   (c (cache-fetch cache 40))
	   (d (cache-fetch cache 25)))
      (5am:is (cleaned-up-p a))
      (5am:is (not (cleaned-up-p b)))
      (5am:is (not (cleaned-up-p c)))
      (5am:is (eq d (cache-fetch cache 25)))
      (5am:is (eq c (cache-fetch cache 40)))
      (5am:is (eq b (cache-fetch cache 30)))
      (cache-fetch cache 22)
      (5am:is (cleaned-up-p d))
      (5am:is (not (cleaned-up-p c)))
      (5am:is (not (cleaned-up-p b)))
      (cache-sanity-check cache))))

;;; MRU: Discard the Most Recently Used entry

(defclass mru-replacement-policy (lifo-replacement-policy)
  ())

(defmethod access-entry ((policy mru-replacement-policy) (entry cache-entry))
  (unlink entry)
  (link-after entry (slot-value policy 'head))
  t)

#+5am
(5am:test mru-replacement-policy
  (with-testing-cache (cache 100 :policy :mru)
    (let* ((a (cache-fetch cache 20))
	   (b (cache-fetch cache 30))
	   (c (cache-fetch cache 40))
	   (d (cache-fetch cache 25)))
      (5am:is (cleaned-up-p c))
      (5am:is (not (cleaned-up-p b)))
      (5am:is (not (cleaned-up-p a)))
      (5am:is (eq a (cache-fetch cache 20)))
      (5am:is (eq d (cache-fetch cache 25)))
      (5am:is (eq b (cache-fetch cache 30)))
      (cache-fetch cache 35)
      (5am:is (cleaned-up-p b))
      (cache-sanity-check cache))))

;;; Random: Randomly discard one of the cached items

(defclass random-replacement-policy (array-replacement-policy)
  ())

(defmethod evict-entry ((policy random-replacement-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (data unused) policy
    (when (> (- (length data) (length unused)) 0)
      (let ((e (loop for i = (random (length data))
		     for e = (aref data i)
		     while (null e)
		     finally (return e))))
	(entry-removed policy e)
	e))))

#+5am
(5am:test random-replacement-policy
  (with-testing-cache (cache 100 :policy :fifo)
    (let* ((a (cache-fetch cache 21))
	   (b (cache-fetch cache 22))
	   (c (cache-fetch cache 23))
	   (d (cache-fetch cache 24)))
      (cache-fetch cache 25)
      (5am:is (= (count-if #'cleaned-up-p (list a b c d)) 1))
      (cache-sanity-check cache))))
