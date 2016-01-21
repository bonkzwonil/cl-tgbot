
;; My Autoalias Bot for telegram
;; Based on eddi

(defparameter *token* (read-line (open #p"token.sexp")))  ;; Provide just your token in token.sexp
(defvar *api* "https://api.telegram.org/bot~a/~a")

(defparameter *last-update-id* 0)

(require 'hunchentoot)
(require 'cl-json)
(require 'drakma)
(load "aliaser")


(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))


(defun build-path (uri)
  (format nil "/~a/~a" *token* uri))


(defmacro json-handler ((name url input) args &body body)
  `(progn
       (hunchentoot:define-easy-handler (,name :uri ,url) ,args
	 (let ((,input (cl-json:decode-json-from-string (hunchentoot:raw-post-data :force-text t))))
	   (setf (hunchentoot:content-type*) "application/json")
	   (cl-json:encode-json-to-string
	    (progn
	      ,@body))))))



(json-handler (echo "/echo" json) ()
  json)

(json-handler (features "/features" json) ()
  *features*)


(json-handler (updatehook (concatenate 'string "/hook/") json) ()
  )

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
  `(cl-json:decode-json (drakma:http-request (build-url ,name ,parameters) :method ,(if content :post :get) :content-type (if ,content "application/json" nil) :want-stream T :content ,content
			 ,@body)))


(defmacro defapi (call (json) &body body)
  `(defun ,call (&key parameters jsonbody)
     (let ((,json (call-api (cl-json:lisp-to-camel-case (symbol-name ',call)) parameters jsonbody)))
       ,@body)))




(defapi get-me (json)
  (format t "~a" json)
  json)
 

(defapi get-updates (json)
  json)


(defapi send-message (json)
  json)

(defun send-message-via-api (chat-id text)
  (format t "Sending ~a to ~a" text chat-id)
  (send-message :jsonbody (cl-json:encode-json-to-string  `(("chat_id" . ,chat-id)
							    ("text" . ,text)))))

(defmacro asva (item place)
  "assoc value"
  `(cdr (assoc ,item ,place)))

(defun count-words (text)
  (1+ (count-if #'(lambda (x) (char= x #\Space)) text)))

(defun handle-updates (update)
  (let ((new-id (asva :update--id (car (last (asva :result update))))))
    (when new-id (setf *last-update-id* (1+ new-id)))
    (mapcar #'handle-update (asva :result update))))


(defun handle-alias (alias chat-id)
  (let ((text (aliaser::lookup alias)))
    (when text
      (send-message-via-api chat-id text))))

(defun handle-update (update)
  (let* ((text (asva :text (asva :message update)))
	 (chat-id (asva :id (asva :chat (asva :message update))))
	 (n-words (count-words text)))
    (cond ((> n-words 2) (aliaser::save text))
	  ((= n-words 1) (handle-alias text chat-id)))))



(defun poll ()
  (handle-updates (get-updates :parameters `( ("offset" . ,*last-update-id*)))))

;; (handle-updates *res*)

;; (trace aliaser::save)
;; (trace aliaser::lookup)
;; (trace handle-text)
;; (trace handle-update)
;; (trace handle-alias)
;; (trace build-url)
;; (trace send-message)
;; (disassemble #'hunchentoot:start)

