(in-package #:teleregilo-automation)

(defmethod render-phrase ((language (eql :eo)) phrase &rest args)
  (declare (ignore args))
  "Atentu, nekonata frazo")

(defmethod render-phrase ((language (eql :en)) phrase &rest args)
  (declare (ignore args))
  "Warning, unknown phrase")

#|
(defhandlers
    ((nil :key_ok)
     (vom:info "key-ok in basis"))
    ((nil :key_tv)
     (vom:info "key-tv in basis")))
|#

(defmethod render-phrase ((language (eql :en)) phrase &rest args)
  "You fool! You have launched the missiles!")

(defmethod render-phrase ((language (eql :eo)) phrase &rest args)
  "Stultulo, vi lanĉis la raketojn!")

(defhandlers
    ((nil :key_media)
     (say 'launched-missiles))
    ((nil :key_text)
     (setf *current-language*
           (case *current-language*
             (:eo :en)
             (:en :eo)))))
