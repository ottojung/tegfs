
(let ((status (system "command -v exiftool >/dev/null 2>/dev/null")))
  (define code (status:exit-val status))
  (unless (= 0 code)
    (throw 'plugin-initialization-failed "No exiftool program found. Please install it first if you want to use the exif-remover plugin")))

(lambda (config root plugins entry generic-fullpath)
  (define preview?
    (string-prefix? generic-fullpath (string-append root "/cache")))
  (and (not preview?)
       (or
        (string-suffix ".jpg" generic-fullpath)
        (string-suffix ".jpeg" generic-fullpath)
        (string-suffix ".png" generic-fullpath)
        (string-suffix ".gif" generic-fullpath))
       (let ()
         (define id (cdr (assq 'id entry)))
         (define path-to-removed
           (string-append root "/temporary/noexif-" id))
         (if (file-exists? path-to-removed)
             path-to-removed
             (let ((status (system* (string-append "exiftool" "-all=" "-o" path-to-removed root))))
               (define code (status:exit-val status))
               (unless (= 0 code)
                 (throw 'plugin-failed "Exiftool exited with non-zero code, which means that it failed."))
               path-to-removed)))))
