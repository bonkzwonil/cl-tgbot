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
  
(defun save (text &optional section)
  (let ((alias (build (cl-ppcre:split " " text))))
    (store (if section (concatenate 'string section ":" alias) alias) text)))


(defun lookup (alias &optional section)
  (restore (if section (concatenate 'string section ":" alias) alias)))





;; Dummy Store
(defparameter *storage* (make-hash-table :test 'equalp))

(defun store (key value)
  (setf (gethash key *storage*) value))

(defun restore (key)
  (gethash key *storage*))


;; Btree storage
(require :cl-btree-0.5)
(b-tree:open #p"data.dat" :if-does-not-exist :create "~{~A~^~}" words))

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


;; Btree storage
(require :cl-btree-0.5)
(b-tree:open #p"data.dat" :if-does-not-exist :create "~{~A~^~}" words))

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


;; redis storage
(require :cl-redis)
(redis:connect)
(red:ping)

(defun store (key value)
  (red:set key value))

(defun restore (key)
  (red:get key))

(save "hallo welt" "bla")
(lookup "hw")
