;;;; cl-journal.asd

(asdf:defsystem #:cl-phash
  :description "Implementation of persistent hash structure"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :license "Public Domain"
  :serial t
  :depends-on (#:split-sequence #:bit-smasher)
  :components ((:file "package")
               (:file "cl-phash")
               (:file "benchmark")
               ))

