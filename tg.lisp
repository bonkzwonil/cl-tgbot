(defpackage cl-tgbot
  (:nicknames tgbot)
  (:use :cl))

(in-package :cl-tgbot)


(defvar *api* "https://api.telegram.org/bot~a/~a")

(defvar  *token* "Please set this tot your telegram token")

(defvar *last-update-id* 0)

(defvar *long-poll-timeout* (* 60 5)) ;5 hours

(setf drakma::*drakma-default-external-format* 'utf-8)
(setf yason::*parse-object-as* :hash-table)

; Our little minilanguage for defining api
(defmacro call-api (name &optional parameters content &body body)
  `(if (null *token*) (error "No API Token set! Please set the variable *token* to your telegram bot token")
       (yason:parse (drakma:http-request (build-url ,name ,parameters) :connection-timeout (+ *long-poll-timeout* 10) :method ,(if content :post :get) :content-type (if ,content "application/json" nil) :want-stream T :content ,content
				     ,@body))))

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



(defun send-message-via-api (chat-id text)
  (let ((text (cl-ppcre:regex-replace-all "\"" text "\\\"")))
    (format t "Sending ~a to ~a~%" text chat-id)
    (send-message :jsonbody (format nil "{\"chat_id\": ~a, \"text\": \"~a\"}" chat-id text))))



(defun hashpath (path hash-table)
  "gets a path in hashtable: eg: '(A B) gets the A key in hash-table and then the B key"
  (loop for key in path
       with value = hash-table
       finally (return value)
     do
       (setf value (gethash key value))))

(defmacro asva (item place)
  "assoc value"
  `(cdr (assoc ,item ,place)))

(defun count-words (text)
  (1+ (count-if #'(lambda (x) (char= x #\Space)) text)))

(defun handle-updates (updates handler)
  (when (gethash "result" updates)
    (let ((new-id (gethash "update_id" (car (last (gethash "result" updates))))))
      (when new-id (setf *last-update-id* (1+ new-id)))
      (mapcar handler (gethash "result" updates)))))



