;;;; package.lisp

(defpackage #:cl-phash
  (:use #:cl #:bit-smasher)
  (:export :empty-map :m-get :m-set :m-del :m-keys :m-count))

(defpackage #:cl-phash/benchmark
  (:use #:cl #:split-sequence #:cl-phash))
