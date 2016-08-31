(in-package #:teleregilo-automation)

(defparameter *current-language* :en)

(defparameter +secret+
  (with-open-file (in (asdf:system-relative-pathname 'teleregilo  "secret.sexp"))
    (read in)))

(defparameter +lumivo-api+ "http://localhost:8005")

(defparameter +enable-motion-api+ "http://localhost:8081/0/detection/start")
(defparameter +disable-motion-api+ "http://localhost:8081/0/detection/pause")
(defparameter +query-motion-api+ "http://localhost:8081/1/detection/status")

(defmacro say (form)
  (alexandria:with-gensyms (lang phrase)
    `(multiple-value-bind (,lang ,phrase) ,form
       (teleregilo-speak:say ,lang ,phrase))))

(defmethod handle-command (mode (cmd (eql :key_power)))
  (setf *mode* nil)
  (setf *stack* nil)
  (say (reset)))

(defun key-to-digit (key)
  (macrolet ((to-digit (var)
               `(case ,var
                  ,@(loop for i from 0 to 9
                       collect `((,(intern (format nil "KEY_~a" i) 'keyword)) ,i)))))

    (to-digit key)))

;; number entry
(defmethod handle-command :before (mode cmd)
  (let ((digit (key-to-digit cmd)))
    (when digit
      (say (digit digit))
      (if (numberp (first *stack*))
          (setf (car *stack*)
                (+ digit
                   (* 10 (car *stack*))))
          (push digit *stack*)))))

(defmethod handle-command (mode (cmd (eql :key_power)))
  (setf *mode* nil)
  (setf *stack* nil)
  (say (reset)))

(defmacro lumivo-post-on-input (name)
  `(chain (request (format nil "~a/inputs/~a" +lumivo-api+ ,name) :method :post)
     #+nil (:attach (body code)
              (declare (ignore body))
              (say (yes-no (eql code 200))))
     (:catcher ()
               (say (someerror)))))

(defhandlers
    
    ((nil :key_red)
     (lumivo-post-on-input "bureau"))
    
    ((nil :key_yellow)
     (lumivo-post-on-input "slaapkamer"))

  ((nil :key_blue)
   (lumivo-post-on-input "midden"))
  ((nil :key_green)
   (lumivo-post-on-input "keuken"))

  ;; start alarm
  ((nil :key_record)
   (chain (request +enable-motion-api+)
     (:catcher ()
               (say (someerror)))
     (:finally ()
       (say (alarm-ready)))))

  ;; stop alarm
  ((nil :key_stop)
   (let ((code (pop *stack*)))
     (if (eql (cl-totp:totp +secret+) code)
         (chain (request +disable-motion-api+)
           (:catcher ()
                     (say (someerror)))
           (:finally ()
             (say (accepted))))
         (say (denied)))))

  ;; alarm on?
  ((nil :key_pause)
   (chain (request +query-motion-api+ :return-body t)
     (:attach (body) (say (yes-no (search "ACTIVE" (babel:octets-to-string body)))))
     (:catcher () (say (someerror)))))
  
  ((nil :key_media)
   (say (launched-missiles)))
  ((nil :key_epg)
   (say (time-now)))
  ((nil :key_text)
   (setf *current-language*
         (case *current-language*
           (:eo (progn (say (language :en)) :en))
           (:en (progn (say (language :eo)) :eo))))))

(defun time-en (universal-time)
  (multiple-value-bind (s m h) (decode-universal-time universal-time)
    (declare (ignore s))
    (format nil "~r ~a~r" h (if (< m 10) "oh " "") m)))


#+nil (defspeech
    (launched-missiles
     :eo
     :en)
    (licht-timer
     :eo "Lumo en ~a ĝis "
     :en "")
  (keuken
   :eo "kuirejo"
   :en "kitchen")
  (midden
   :eo "mezo"
   :en "centre")
  (slaapkamer
   :eo "dormĉambro"
   :en "bedroom")
  (bureau
   :eo "skribotablo"
   :en "desk")
  )

(defun digit (num)
  (values *current-language*
          (ecase *current-language*
            ((:en) (format nil "~r" num))
            ((:eo) (aref #("nul" "unu" "du" "tri" "kvar" "kvin" "ses" "sep" "ok" "naŭ") num)))))

(defun yes-no (p)
  (values *current-language*
          (ecase *current-language*
            ((:en) (if p "Yes" "No"))
            ((:eo) (if p "Jes" "Ne")))))

(defun accepted ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "Accepted")
            ((:eo) "Akceptite"))))

(defun denied ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "Denied")
            ((:eo) "Rifuzite"))))

(defun someerror ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "Error")
            ((:eo) "Eraro"))))

(defun alarm-ready ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "Alarm ready")
            ((:eo) "Alarmo pretas"))))

(defun alarm ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "Alarm")
            ((:eo) "Alarmo"))))

(defun reset ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "Reset")
            ((:eo) "Komenca stato"))))

(defun language (lang)
  (values lang
          (ecase lang
            ((:en) "English")
            ((:eo) "Esperanto"))))

(defun launched-missiles ()
  (values *current-language*
          (ecase *current-language*
            ((:en) "You fool! You have launched the missiles!")
            ((:eo) "Stultulo, vi lanĉis la raketojn!"))))

(defun time-now ()
  (values *current-language*
          (ecase *current-language*
            ((:en) (format nil "Time now is ~a" (time-en (get-universal-time))))
            ((:eo) (format nil "Horo estas la ~a" (time-eo (get-universal-time)))))))

(defun time-en (universal-time)
  (multiple-value-bind (s m h) (decode-universal-time universal-time)
    (declare (ignore s))
    (format nil "~r ~a~r" h (if (< m 10) "oh " "") m)))

(defun time-eo (universal-time)
  (multiple-value-bind (s m h) (decode-universal-time universal-time)
    (declare (ignore s))
    (format nil "~aa kaj ~a" (eo-numeroj h) (eo-numeroj m))))

(defun en-numbers (input)
  (format nil "~r" input))

(defun eo-numeroj (input &key (skalvortoj-p t))
  "Faris CoselTech, permesilo GPL"
  (labels ((kalkulu (input lst)
             (loop
                with nums = #("" "unu" "du" "tri" "kvar" "kvin" "ses" "sep" "ok" "naŭ")
                with grs = #("" "dek" "cent")
                with grps = #(""  "mil" "miliono" "miliardo" "duiliono" "duiliardo" "triiliono" "triiliardo" "kvariliono" "kvariliardo"  "kviniliono" "kviniliardo" "sesiliono" "sesiliardo" "sepiliono" "sepiliardo" "okiliono" "okiliardo" "naŭiliono" "naŭiliardo" "dekiliono" "dekiliardo")

                for i = 0 then (1+ i)
                for (grp pos) = (multiple-value-list (floor i 3))
                  
                for (gtail ghead) = (multiple-value-list (if (zerop pos)
                                                             (floor (or gtail input) 1000)
                                                             (values gtail ghead)))
                for (tail head) = (multiple-value-list (floor (or tail input) 10))

                for num = (aref nums head) 
                for gr = (aref grs pos)

                for grupo-p = (and (zerop pos) (> grp 0)
                                   (or (plusp ghead) (zerop gtail)))
                for numeralo-p = (and (plusp head)
                                      (or (< 1 head)
                                          (and (zerop pos) (< 1 ghead))
                                          (= i 0)))
                for subgrupo-p = (and (plusp head) (plusp pos))

                for acc = lst then post-numeralo 
                for komenco = (if pad-p (cons " " acc) acc)

                for post-grupo = (if grupo-p
                                     (if (and skalvortoj-p (< grp 22))
                                         (list* (aref grps grp)
                                                komenco)
                                         (list* "oble dek alt " (kalkulu i (cons " kaj" komenco))))
                                     komenco)
                  
                for post-subgrupo = (if subgrupo-p
                                        (cons gr post-grupo)
                                        post-grupo)

                for post-numeralo = (if numeralo-p
                                        (if (and grupo-p (not subgrupo-p))
                                            (list* num " " post-subgrupo)
                                            (cons num post-subgrupo))
                                        post-subgrupo)

                for pad-p = (or numeralo-p grupo-p subgrupo-p)
                  
                until (zerop tail)
                finally (return (if (plusp input)
                                    post-numeralo
                                    (list* "nul" lst))))))
    (apply #'concatenate 'string (kalkulu input nil))))
