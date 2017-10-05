(in-package #:cacle)

(defclass cache-entry ()
  ((key :initarg :key :reader entry-key)
   (data)
   (pending :initarg :pending)
   (size :reader entry-size)
   (expiry :reader entry-expiry)))

(defmethod print-object ((obj cache-entry) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ "key " stream)
    (prin1 (slot-value obj 'key) stream)))

(defmethod entry-valid-p ((entry cache-entry))
  (slot-boundp entry 'size))

(defclass linked-cache-entry (cache-entry)
  ((next :reader entry-next)
   (prev :reader entry-previous)))

(defmethod shared-initialize ((entry linked-cache-entry) slot-names &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (when (or (eq slot-names t)
	    (find 'next slot-names))
    (setf (slot-value entry 'next) entry))
  (when (or (eq slot-names t)
	    (find 'prev slot-names))
    (setf (slot-value entry 'prev) entry)))

(defmethod unlink ((entry linked-cache-entry))
  (let ((n (slot-value entry 'next))
	(p (slot-value entry 'prev)))
    (when (and (eq n entry)
	       (eq p entry))
      (error "Attempt to unlink an already unlinked entry ~s" entry))
    (setf (slot-value n 'prev) p
	  (slot-value p 'next) n
	  (slot-value entry 'next) entry
	  (slot-value entry 'prev) entry)
    entry))

(defun ensure-unlinked (entry)
  (with-slots (next prev)
      entry
    (unless (and (eq next entry)
		 (eq prev entry))
      (error "Attempt to link an already linked entry ~s" entry))))

(defmethod link-before ((entry linked-cache-entry) (ref linked-cache-entry))
  (ensure-unlinked entry)
  (let ((n ref)
	(p (slot-value ref 'prev)))
    (setf (slot-value p 'next) entry
	  (slot-value n 'prev) entry
	  (slot-value entry 'next) n
	  (slot-value entry 'prev) p)
    entry))

(defmethod link-after ((entry linked-cache-entry) (ref linked-cache-entry))
  (ensure-unlinked entry)
  (let ((n (slot-value ref 'next))
	(p ref))
    (setf (slot-value p 'next) entry
	  (slot-value n 'prev) entry
	  (slot-value entry 'next) n
	  (slot-value entry 'prev) p)
    entry))

(defclass indexed-cache-entry (cache-entry)
  ((index :accessor entry-index)))
