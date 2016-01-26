
;; My Autoalias Bot for telegram
;; Based on eddi's libs :)

(in-package :cl-tgbot)

(defparameter *token* (read-line (open #p"token.txt")))  ;; Provide just your token in token.txt

(defun handle-alias (alias chat-id)
  (let ((text (aliaser::lookup alias (format nil "~a" chat-id))))
    (when text
      (send-message-via-api chat-id text))))

(defun handle-update (update)
  (let* ((text (hashpath '("message" "text") update))
	 (chat-id (hashpath '("message" "chat" "id") update))
	 (n-words (count-words text)))
    (cond ((> n-words 2) (aliaser::save text (format nil "~a" chat-id)))
	  ((= n-words 1) (handle-alias text chat-id)))))



(defun poll ()
  (handler-case (handle-updates (get-updates :parameters `( ("limit" . 5) ("offset" . ,*last-update-id*) ("timeout" . 1590))) #'handle-update)
    (condition (e) (format t "Unexpected condition: ~a" e))))



(defun run ()
  (loop (poll) (sleep 1)))



;; (handle-updates *res*)

(trace aliaser::save)
(trace aliaser::lookup)
(trace handle-text)
(trace handle-update)
(trace handle-alias)
(trace build-url)
(trace send-message)
;; (disassemble #'hunchentoot:start)

