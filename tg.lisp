(defpackage cl-tgbot 
  (:use :cl))

(in-package :cl-tgbot)


(defvar *api* "https://api.telegram.org/bot~a/~a")

(defparameter *last-update-id* 0)

(require 'cl-json)
(require 'drakma)



; Our little minilanguage for defining api
(defmacro defapi (call (json) &body body)
  `(defun ,call (&key parameters jsonbody)
     (let ((,json (call-api (cl-json:lisp-to-camel-case (symbol-name ',call)) parameters jsonbody)))
       ,@body)))


(defapi get-me (json)  ;; Gets automatically expanded to getMe etcp... :)
  json)
 

(defapi get-updates (json)
  json)


(defapi send-message (json)
  json)


;; API done :)

(defun url-encode (text)
  (cl-ppcre:regex-replace-all "\\+"
			      (drakma:url-encode (format nil "~a" text) :UTF8)
			      "%20"))
			      
  

(defun build-url (endpoint &optional parameters)
  "WHY cant drakma do it?? Edi?"
  (let ((url (format nil *api* *token* endpoint)))
    (if parameters
	(format nil "~a?~a" url (format nil "~{~A~^&~}"
					(mapcar #'(lambda (x) (format nil "~a=~a"
								      (url-encode (car x))
								      (url-encode (cdr x))))
						parameters)))
	url)))


(defmacro call-api (name &optional parameters content &body body)
  `(cl-json:decode-json (drakma:http-request (build-url ,name ,parameters) :connection-timeout 6000 :method ,(if content :post :get) :content-type (if ,content "application/json" nil) :want-stream T :content ,content
			 ,@body)))




(defun send-message-via-api (chat-id text)
  (format t "Sending ~a to ~a" text chat-id)
  (send-message :jsonbody (cl-json:encode-json-to-string  `(("chat_id" . ,chat-id)
							    ("text" . ,text)))))

(defmacro asva (item place)
  "assoc value"
  `(cdr (assoc ,item ,place)))

(defun count-words (text)
  (1+ (count-if #'(lambda (x) (char= x #\Space)) text)))

(defun handle-updates (updates handler)
  (let ((new-id (asva :update--id (car (last (asva :result updates))))))
    (when new-id (setf *last-update-id* (1+ new-id)))
    (mapcar handler (asva :result updates))))
