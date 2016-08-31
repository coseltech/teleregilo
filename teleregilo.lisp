;;;; teleregilo.lisp

(in-package #:teleregilo)

(defparameter +lircd-socket-pathname+ #p"/var/run/lirc/lircd")

;;; data types

(defclass lirc-line ()
  ((hex :initarg :hex :accessor hex)
   (repeat :initarg :repeat :accessor repeat)
   (key-name :initarg :key-name :accessor key-name)
   (device :initarg :device :accessor device)))

(defmethod print-object ((object lirc-line) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (hex repeat key-name device) object
      (format stream "~a ~a ~a ~a" hex repeat key-name device))))

;;; control

(defvar *mode* nil)
(defvar *stack* nil)

(defun current-mode ()
  (first *mode*))

(defgeneric handle-command (mode cmd))

(defmethod handle-command (mode cmd)
  (vom:info "Fallthrough handle cmd mode ~a cmd ~a" mode cmd))

(defmacro defhandlers (&rest handlers)
  `(progn ,@(loop for item in handlers
                collect (destructuring-bind ((mode cmd) &rest body) item
                          `(defmethod handle-command ((,(gensym) (eql ,mode)) (,(gensym) (eql ,cmd)))
                             ,@body)))))

;;; thread & locks

(defvar *lirc-thread* nil)
(defvar *lock* (bt:make-lock))
(defvar *exit-notifier* nil)

(defun thread-running-p ()
  "Returns true if the event thread is running"
  (bt:with-lock-held (*lock*)
    (when *lirc-thread* t)))

(defun stop ()
  (when *exit-notifier* (as:trigger-notifier *exit-notifier*))
  (values))

(defun handle-lirc-event (line)
  (vom:info "~a" line)
  (with-slots (key-name) line
      (handle-command (current-mode)
                      (intern (string-upcase key-name) :keyword))))

(defun start (&optional (lircd-pathname +lircd-socket-pathname+))
  (unless *lirc-thread* 
    (let ((loop-ready-lock (bt:make-lock))
          (loop-ready (bt:make-condition-variable)))
      (setf *lirc-thread*
            (bt:make-thread (lambda ()
                              (unwind-protect 
                                   (as:with-event-loop ()
                                     (let ((lines '())
                                           (tail "")
                                           (new-lines-notifier))
                                       (as:add-event-loop-exit-callback
                                        (lambda ()
                                          (bt:with-lock-held (*lock*)
                                            (dolist (notifier (list *exit-notifier* new-lines-notifier))
                                              (unless (as:notifier-freed-p notifier)
                                                (as:free-notifier notifier)))
                                            (setf *lirc-thread* nil
                                                  *exit-notifier* nil))))
                                       (format *debug-io* "~&;; event thread started.~%")
                                       (setf new-lines-notifier
                                             (as:make-notifier
                                              (lambda ()
                                                (dolist (line lines)
                                                  (destructuring-bind (hex repeat key-name device)
                                                      (loop for i = 0 then (1+ j)
                                                         as j = (position #\Space line :start i)
                                                         collect (subseq line i j)
                                                         while j)
                                                    (handle-lirc-event
                                                     (make-instance 'lirc-line
                                                                    :hex (parse-integer hex :radix 16)
                                                                    :repeat (parse-integer repeat :radix 16)
                                                                    :key-name key-name
                                                                    :device device)))))
                                              :single-shot nil))
                                       (as:pipe-connect 
                                        lircd-pathname
                                        (lambda (sock data)
                                          (declare (ignore sock))
                                          (setf lines
                                                (loop with incoming = (concatenate 'string tail (babel:octets-to-string data))
                                                   for i = 0 then (1+ j)
                                                   as j = (position #\Newline incoming :start i)
                                                   while j collect (subseq incoming i j)
                                                   finally (setf tail (subseq incoming i))))
                                          (as:trigger-notifier new-lines-notifier)))
                                       (bt:with-lock-held (loop-ready-lock)
                                         (setf *exit-notifier* (as:make-notifier #'(lambda () (as:exit-event-loop))))
                                         (bt:condition-notify loop-ready))))
                                (format *debug-io* "~&;; event thread exited.~%")))
                            :name "LIRC socket thread"))
      (bt:with-lock-held (loop-ready-lock)
        (loop until *exit-notifier* do (bt:condition-wait loop-ready loop-ready-lock)))))
  (values))
