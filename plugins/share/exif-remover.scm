
(let ((status (system "command -v exiftool >/dev/null 2>/dev/null")))
  (define code (status:exit-val status))
  (unless (= 0 code)
    (throw 'plugin-initialization-failed "No exiftool program found. Please install it first if you want to use the exif-remover plugin")))

(define (path-extension str)
  (let ((index (string-index-right str #\.)))
    (if index
        (let ((ext (string-drop str index)))
          (if (< 1 (string-length ext)) ext ""))
        "")))

(lambda (config root entry fullpath)
  (define preview-prefix (string-append root "/cache"))
  (define preview? (string-prefix? preview-prefix fullpath))
  (and (not preview?)
       (let ((mimetype (cdr (or (assq 'mimetype entry) (cons 'mimetype "unknown")))))
         (string-prefix? "image/" mimetype))
       (let ()
         (define id (cdr (assq 'id entry)))
         (define extension (path-extension fullpath))
         (define path-to-removed
           (string-append root "/tmp/noexif-" id extension))
         (if (file-exists? path-to-removed)
             path-to-removed
             (let ((status (system* "exiftool" "-all=" "-o" path-to-removed fullpath)))
               (define code (status:exit-val status))
               (unless (= 0 code)
                 (throw 'plugin-failed "Exiftool exited with non-zero code. Image sharing must not go trough." id))
               path-to-removed)))))
