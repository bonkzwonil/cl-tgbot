(require 'hunchentoot)

(defun build-path (uri)
  (format nil "/~a/~a" *token* uri))


(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))


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
