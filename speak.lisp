(in-package #:teleregilo-speak)

(defconstant +festival-password+ "H70cihxIbpcQzFnRpzeoadAcJDmd6tA7oWCpjSvDY3PVD8PAq66B7QPsK5yN69dALxpvMM6sdfePs5TzLjpsV28cmmmwtmHFitatVmNv8cKntDmxs9DQoWd33RG7ifBP")

(defvar *current-language* :en)

(defvar *festival-socket* nil)

(defgeneric render-phrase (language phrase &rest args))

(defmethod festival-preamble (language)
  (case language
    (:en (format nil "(language_american_english)~%"))
    (:eo (format nil "(language_esperanto)~%"))))

(defun ensure-festival-connected ()
  (vom:info "Socket ~a" *festival-socket*)
  (unless *festival-socket*
    (setf *festival-socket*
          (as:tcp-connect "localhost" 1314
                          (lambda (socket data)
                            (declare (ignore socket))
                            (vom:info "Festival said ~a" (babel:octets-to-string data)))
                          :event-cb (lambda (err)
                            (vom:info "Festival connection error ~a" err)
                            (setf *festival-socket*  nil))
                          :data (format nil "~a~%" +festival-password+)))
      (vom:info "Socket now ~a" *festival-socket*)))

(defun festival (language message)
  (let ((data (format nil "~a(SayText \"~a\")~%"
                                (festival-preamble language)
                                message)))
    (vom:info "To festival: ~a" data)
    (ensure-festival-connected)
    (as:write-socket-data *festival-socket* data)))

(defun say (phrase &rest args)
  (vom:info "Saying ~a" phrase)
  (festival *current-language* (funcall #'render-phrase *current-language* args)))


