(in-package #:teleregilo-speak)

(defparameter +festival-password+ "H70cihxIbpcQzFnRpzeoadAcJDmd6tA7oWCpjSvDY3PVD8PAq66B7QPsK5yN69dALxpvMM6sdfePs5TzLjpsV28cmmmwtmHFitatVmNv8cKntDmxs9DQoWd33RG7ifBP")

(defvar *festival-socket* nil)

(defmethod festival-preamble (language)
  (case language
    (:en (format nil "(language_american_english)~%"))
    (:eo (format nil "(language_esperanto)~%"))))

(defun ensure-festival-connected ()
  (vom:info "Socket ~a" *festival-socket*)
  (unless (and *festival-socket* (not (as:socket-closed-p *festival-socket*)))
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

(defun say (language phrase)
  (vom:info "Saying ~a" phrase)
  (festival language phrase))


