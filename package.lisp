;;;; package.lisp

(defpackage #:teleregilo
  (:use #:cl)
  (:export #:*mode*
           #:*stack*
           #:handle-command
           #:defhandlers
           #:start-thread
           #:stop-thread)
  

