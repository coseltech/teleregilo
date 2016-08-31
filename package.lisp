;;;; package.lisp

(defpackage #:teleregilo
  (:use #:cl)
  (:export #:*mode*
           #:*stack*
           #:handle-command
           #:defhandlers
           #:start
           #:stop))

(defpackage #:teleregilo-speak
  (:use #:cl)
  (:export #:say))

(defpackage #:teleregilo-automation
  (:use #:cl 
        #:teleregilo
        #:blackbird
        #:carrier))

