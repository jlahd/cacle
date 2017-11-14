(in-package #:cacle)

#+5am
(5am:in-suite cacle-tests)

;;; Heap structure for LFU policies

(defclass heap-cache-entry (indexed-cache-entry)
  ((weight :accessor cache-entry-weight)))

(defmethod print-object ((obj heap-cache-entry) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ "key " stream)
    (prin1 (slot-value obj 'key) stream)
    (princ " weight " stream)
    (prin1 (slot-value obj 'weight) stream)))

(defun heap-parent-idx (idx)
  (floor (1- idx) 2))

(defun heap-left-idx (idx)
  (1+ (* idx 2)))

(defun heap-right-idx (idx)
  (* (1+ idx) 2))

(defun heap-parent (heap idx)
  (and (> idx 0)
       (aref heap (heap-parent-idx idx))))

(defun heap-left (heap idx)
  (let ((left (heap-left-idx idx)))
    (and (< left (length heap))
	 (aref heap left))))

(defun heap-right (heap idx)
  (let ((right (heap-right-idx idx)))
    (and (< right (length heap))
	 (aref heap right))))

(defun heap-swap (heap i1 i2)
  (let ((e1 (aref heap i1))
	(e2 (aref heap i2)))
    (setf (entry-index e1) i2
	  (entry-index e2) i1
	  (aref heap i1) e2
	  (aref heap i2) e1)
    (values e2 e1)))

(defun sink-down (heap idx &optional prefer-to-sink)
  (let ((me (aref heap idx))
	(left (heap-left heap idx))
	(right (heap-right heap idx)))
    (unless (and (or (null left)
		     (< (cache-entry-weight me)
			(cache-entry-weight left))
		     (and (not prefer-to-sink)
			  (= (cache-entry-weight me)
			     (cache-entry-weight left))))
		 (or (null right)
		     (< (cache-entry-weight me)
			(cache-entry-weight right))
		     (and (not prefer-to-sink)
			  (= (cache-entry-weight me)
			     (cache-entry-weight right)))))
      ;; heavier than (one of) children, do sink
      (let ((lightest (if (and right
			       (< (cache-entry-weight right)
				  (cache-entry-weight left)))
			  (heap-right-idx idx)
			  (heap-left-idx idx))))
	(heap-swap heap idx lightest)
	(sink-down heap lightest prefer-to-sink)))))

(defun bubble-up (heap idx)
  (let ((me (aref heap idx))
	(parent (heap-parent heap idx)))
    (unless (or (null parent)
		(>= (cache-entry-weight me)
		    (cache-entry-weight parent)))
      ;; lighter than parent, do bubble
      (let ((p (heap-parent-idx idx)))
	(heap-swap heap idx p)
	(bubble-up heap p)))))

;;; Discard the Least Frequenty Used entry

(defclass lfu-replacement-policy (replacement-policy)
  ((heap :initform (make-array 16 :adjustable t :fill-pointer 0))))

(defmethod entry-added ((policy lfu-replacement-policy) (entry cache-entry))
  (change-class entry 'heap-cache-entry)
  (with-slots (heap) policy
    (setf (cache-entry-weight entry) 1
	  (entry-index entry) (vector-push-extend entry heap))
    (bubble-up heap (entry-index entry))))

(defmethod access-entry ((policy lfu-replacement-policy) (entry heap-cache-entry))
  (incf (cache-entry-weight entry))
  (sink-down (slot-value policy 'heap) (entry-index entry) t)
  t)

(defmethod entry-removed ((policy lfu-replacement-policy) (entry heap-cache-entry))
  (with-slots (heap) policy
    (let ((i (entry-index entry)))
      (setf (entry-index entry) nil)
      (unless (= i (1- (length heap)))
	(setf (aref heap i) (vector-pop heap)
	      (entry-index (aref heap i)) i)
	(sink-down heap i)))))

(defmethod evict-entry ((policy lfu-replacement-policy) size-hint)
  (declare (ignore size-hint))
  (with-slots (heap) policy
    (when (> (length heap) 0)
      (let* ((lightest (aref heap 0))
	     (heaviest (vector-pop heap)))
	(when (> (length heap) 0)
	  (setf (aref heap 0) heaviest
		(entry-index heaviest) 0)
	  (sink-down heap 0 t))
	lightest))))

#+5am
(defmethod list-entries ((policy lfu-replacement-policy))
  (coerce (slot-value policy 'heap) 'list))

#+5am
(5am:test lfu-replacement-policy
  (with-testing-cache (cache 100 :policy :lfu)
    (let* ((a (cache-fetch cache 21))
	   (b (cache-fetch cache 22))
	   (c (cache-fetch cache 23))
	   (d (cache-fetch cache 24)))
      (5am:is (= 0 (count-if #'cleaned-up-p (list a b c d))))
      (cache-fetch cache 21)
      (cache-fetch cache 23)
      (cache-fetch cache 24)
      (cache-fetch cache 23)
      (cache-fetch cache 21)
      (let ((e (cache-fetch cache 25)))
	(5am:is (cleaned-up-p b))
	(cache-fetch cache 26)
	(5am:is (cleaned-up-p e))
	(cache-sanity-check cache)))))

;;; Discard the Least Frequently Used entry (with dynamic aging)

(defclass lfuda-replacement-policy (lfu-replacement-policy)
  ((age :initform 0)))

(defmethod entry-added ((policy lfuda-replacement-policy) (entry cache-entry))
  (call-next-method)
  (incf (cache-entry-weight entry) (slot-value policy 'age))
  (sink-down (slot-value policy 'heap) (entry-index entry)))

(defmethod evict-entry ((policy lfuda-replacement-policy) size-hint)
  (declare (ignore size-hint))
  (let ((target (call-next-method)))
    (when target
      (setf (slot-value policy 'age) (cache-entry-weight target)))
    target))

#+5am
(5am:test lfuda-replacement-policy
  (with-testing-cache (cache 100 :policy :lfuda :item-size-modulus 100)
    (let* ((a (cache-fetch cache 125))
	   (b (cache-fetch cache 225))
	   (c (cache-fetch cache 325))
	   (d (cache-fetch cache 425)))
      (dotimes (i 40)
	(5am:is (eq a (cache-fetch cache 125)))
	(when (>= i 10)
	  (5am:is (eq d (cache-fetch cache 425))))
	(when (>= i 20)
	  (5am:is (eq b (cache-fetch cache 225))))
	(when (>= i 30)
	  (5am:is (eq c (cache-fetch cache 325)))))
      (dotimes (i 5)
	(let ((e (cache-fetch cache 525)))
	  (5am:is (cleaned-up-p c))
	  (setf c (cache-fetch cache 625))
	  (5am:is (cleaned-up-p e))))
      (dotimes (i 100)
	(cache-fetch cache 525)
	(cache-fetch cache 625)
	(cache-fetch cache 725)
	(cache-fetch cache 825))
      (5am:is (= 0 (count-if-not #'cleaned-up-p (list a b d))))
      (cache-sanity-check cache))))

