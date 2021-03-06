(in-package #:cl-user)
(defpackage #:cl-tgbot-system (:use #:cl #:asdf))
(in-package #:cl-tgbot-system)

(defsystem cl-tgbot 
  :name "cl-tgbot"
  :maintainer "Mathias Menzel-Nielsen <mathias@menzel-nielsen.de>"
  :description "Telegram Bot in Common Lisp"
  :depends-on (:hunchentoot
	       :drakma
	       :cl-json
	       :yason
	       :cl-redis
	       :cl-ppcre)
  :components
  ((:file "tg")
   (:file "aliaser")
   (:file "bot" :depends-on ("tg" "aliaser"))))

