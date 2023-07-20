;; This is an example that recalculates all entries' mimetypes.

(cond-expand
 (guile
  (define-module (example recalculate-mimetypes)
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((tegfs entries-map-bang) :select (entries-map!))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs get-file-mimetype) :select (get-file-mimetype))
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path))
    :use-module ((tegfs keyword-mimetype) :select (keyword-mimetype)))))

(entries-map!
 (lambda (entry)
   (define fullpath
     (entry-target-fullpath entry))
   (if (not fullpath) entry
       (let ((new-mimetype (get-file-mimetype fullpath)))
         (assoc-set-value keyword-mimetype new-mimetype entry)))))
