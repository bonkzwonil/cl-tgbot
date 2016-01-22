					; The Aliaser Module


(defpackage :aliaser 
  (:use :cl))

(in-package :aliaser)


(defun join-words (words)
  (format nil "~{~A~^~}" words))

(defun build (words)
  (join-words (mapcar #'(lambda (word) (char word 0))
	  words)))
  
(defun save (text &optional section)
  (let ((alias (build (remove-if #'(lambda (x) (= 0 (length x))) (cl-ppcre:split " " text)))))
    (store (if section (concatenate 'string section ":" alias) alias) text)))


(defun lookup (alias &optional section)
  (restore (if section (concatenate 'string section ":" alias) alias)))




;; ;; Dummy Store
;; (defparameter *storage* (make-hash-table :test 'equalp))

;; (defun store (key value)
;;   (setf (gethash key *storage*) value))

;; (defun restore (key)
;;   (gethash key *storage*))


;; redis storage
(require :cl-redis)
(redis:connect)
(red:ping)

(defun redis-key (key)
  (concatenate 'string "aliaser:" (string-downcase key)))

(defun store (key value)
  (red:set (redis-key key) value))

(defun restore (key)
  (red:get (redis-key key)))

(save "hallo welt👀🌵 " "bla")
(lookup "hw" "bla")
