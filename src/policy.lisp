(in-package #:cacle)

(defclass replacement-policy ()
  ())

(defgeneric entry-added (policy entry))
(defgeneric access-entry (policy entry))
(defgeneric entry-removed (policy entry))
(defgeneric evict-entry (policy size-hint))
