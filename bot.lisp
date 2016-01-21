
;; My Autoalias Bot for telegram
;; Based on eddi's libs :)

(in-package :cl-tgbot)

(defparameter *token* (read-line (open #p"token.txt")))  ;; Provide just your token in token.txt

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
  (handle-updates (get-updates :parameters `( ("offset" . ,*last-update-id*) ("timeout" . 590))) #'handle-update))



(defun run ()
  (loop (poll) (sleep 1)))



;; (handle-updates *res*)

;; (trace aliaser::save)
;; (trace aliaser::lookup)
;; (trace handle-text)
;; (trace handle-update)
;; (trace handle-alias)
;; (trace build-url)
;; (trace send-message)
;; (disassemble #'hunchentoot:start)
