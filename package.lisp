;;;; package.lisp

(defpackage #:teleregilo
  (:use #:cl)
  (:export #:*mode*
           #:*stack*
           #:handle-command
           #:defhandlers
           #:start-thread
           #:stop-thread))

(defpackage #:teleregilo-speak
  (:use #:cl)
  (:export #:*current-language*
           #:render-phrase
           #:say))

(defpackage #:teleregilo-automation
  (:use #:cl 
        #:teleregilo
        #:teleregilo-speak))

