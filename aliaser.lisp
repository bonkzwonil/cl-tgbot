					; The Aliaser Module

(require :cl-ppcre)

(defpackage :aliaser 
  (:use :cl))

(in-package :aliaser)


(defun join-words (words)
  (format nil "~{~A~^~}" words))

(defun build (words)
  (join-words (mapcar #'(lambda (word) (char word 0))
	  words)))
  
(defun save (text)
  (let ((alias (build (cl-ppcre:split " " text))))
    (store alias text)))


(defun lookup (alias)
  (restore alias))





;; Dummy Store
(defparameter *storage* (make-hash-table :test 'equalp))

(defun store (key value)
  (setf (gethash key *storage*) value))

(defun restore (key)
  (gethash key *storage*))



(save "hallo welt")
(lookup "hw")
