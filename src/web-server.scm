;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var tegfs-serve/parse

%use (with-cli define-cli:show-help) "./euphrates/define-cli.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (read-all-port) "./euphrates/read-all-port.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (current-program-path/p) "./euphrates/current-program-path-p.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"
%use (list-zip) "./euphrates/list-zip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (un~s) "./euphrates/un-tilda-s.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (appcomp comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (fn-tuple) "./euphrates/fn-tuple.scm"
%use (fn-cons) "./euphrates/fn-cons.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string-split-3) "./euphrates/string-split-3.scm"
%use (define-type9) "./euphrates/define-type9.scm"
%use (make-hashmap hashmap-ref hashmap-set! hashmap->alist alist->hashmap hashmap-delete! hashmap-foreach) "./euphrates/ihashmap.scm"
%use (list->hashset hashset->list hashset-ref make-hashset hashset-ref hashset-add! hashset-delete!) "./euphrates/ihashset.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (alphanum-lowercase/alphabet) "./euphrates/alphanum-lowercase-alphabet.scm"
%use (printf) "./euphrates/printf.scm"
%use (path-extensions) "./euphrates/path-extensions.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (get-current-directory) "./euphrates/get-current-directory.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (directory-files) "./euphrates/directory-files.scm"
%use (directory-files-rec) "./euphrates/directory-files-rec.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (catch-any) "./euphrates/catch-any.scm"
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (assoc-or) "./euphrates/assoc-or.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"

%use (get-root) "./get-root.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (tegfs-process-categorization-text) "./edit-tags.scm"
%use (tegfs-add) "./add.scm"
%use (tegfs-get/cached) "./get.scm"
%use (tegfs-query) "./query.scm"
%use (sha256sum) "./sha256sum.scm"
%use (parse-multipart-as-hashmap) "./web-parse-multipart.scm"
%use (web-make-response) "./web-make-response.scm"
%use (get-random-basename) "./get-random-basename.scm"
%use (web-set-cookie-header) "./web-set-cookie-header.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-style) "./web-style.scm"
%use (web-login-body) "./web-login-body.scm"
%use (web-login-failed-body) "./web-login-failed-body.scm"
%use (web-login-success-body) "./web-login-success-body.scm"
%use (web-message-template) "./web-message-template.scm"
%use (web-make-upload-body) "./web-make-upload-body.scm"
%use (web-sendfile) "./web-sendfile.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"
%use (web-preview-height) "./web-preview-height.scm"
%use (web-preview-width) "./web-preview-width.scm"
%use (tegfs-make-thumbnails) "./make-thumbnails.scm"
%use (read-file-head) "./read-file-head.scm"
%use (get-config) "./get-config.scm"
%use (get-preview-path) "./get-preview-path.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (entry-parent-directory-key) "./entry-parent-directory-key.scm"
%use (entry-parent-directory-vid-key) "./entry-parent-directory-vid-key.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (web-url-icon/svg) "./web-url-icon-svg.scm"
%use (standalone-file->entry/prefixed) "./standalone-file-to-entry.scm"
%use (web-request-get-domainname) "./web-request-get-domainname.scm"
%use (web-try-uri-decode) "./web-try-uri-decode.scm"

%use (context-ctr context? context-passwords context-database context-tokens context-port context-fileserver context-sharedir context-filemap/2) "./web-context.scm"
%use (callcontext-ctr callcontext? callcontext-break callcontext-request callcontext-query callcontext-body callcontext-time callcontext-key set-callcontext-key! callcontext-permissions) "./web-callcontext.scm"

%use (debug) "./euphrates/debug.scm"
%use (debugv) "./euphrates/debugv.scm"

%for (COMPILER "guile")

(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (sxml simple))

(use-modules (ice-9 iconv))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 binary-ports))

%end

(define-type9 <sharedinfo>
  (sharedinfo-ctr sourcepath sharedname vid token ctime stime) sharedinfo?
  (sourcepath sharedinfo-sourcepath) ;; the original file path
  (sharedname sharedinfo-sharedname) ;; the linked file path suffix (without the sharedir)
  (vid sharedinfo-vid) ;; unique virtual id
  (token sharedinfo-token) ;; token of the perms that shared this file
  (ctime sharedinfo-ctime) ;; time in seconds for when this info was created
  (stime sharedinfo-stime) ;; time in seconds for how long to share this file
  )

(define-type9 <permission>
  (permission-constructor token start time admin? detailsaccess? filemap idset) permission?
  (token permission-token) ;; token string
  (start permission-start) ;; timestamp for when this token was created
  (time permission-time) ;; duration in secods for how long this token is valid
  (admin? permission-admin?) ;; true if user is an admin
  (detailsaccess? permission-detailsaccess?) ;; true if user has access to objects details
  (filemap permission-filemap) ;; hashmap with `keys: target-fullpath that was shared with this permission` and `values: sharedinfos`
  (idset permission-idset) ;; hashset with `values: id of entry that is shared with this permission`
  )

(define context/p
  (make-parameter #f))

(define callcontext/p
  (make-parameter #f))

(define default-preview-sharing-time
  (string->seconds "30m"))
(define default-full-sharing-time
  (string->seconds "30m"))
(define default-login-expiery-time
  (string->seconds "12h"))
(define default-share-expiery-time
  (string->seconds "1h"))

(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (make-sharedinfo token target-fullpath for-duration)
  (define sharedname
    (string-append
     (get-random-basename)
     (path-extensions target-fullpath)))
  (define callctx (callcontext/p))
  (define now (callcontext-time callctx))
  (define vid (get-random-basename))
  (sharedinfo-ctr target-fullpath sharedname vid token now for-duration))

(define (filemap-set! filemap/2 info)
  (define vid (sharedinfo-vid info))
  (define sharedname (sharedinfo-sharedname info))
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (hashmap-set! first vid info)
  (hashmap-set! second sharedname info))

(define (filemap-ref-by-vid filemap/2 id default)
  (define first (car filemap/2))
  (hashmap-ref first id default))

(define (filemap-ref-by-sharedname filemap/2 sharedname default)
  (define second (cdr filemap/2))
  (hashmap-ref second sharedname default))

(define (filemap-delete-by-vid! filemap/2 id)
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (define info (filemap-ref-by-vid filemap/2 id #f))
  (when info
    (let ((sharedname (sharedinfo-sharedname info)))
      (hashmap-delete! first id)
      (hashmap-delete! second sharedname))))

(define (filemap-delete-by-sharedname! filemap/2 sharedname)
  (define first (car filemap/2))
  (define second (cdr filemap/2))
  (define info (filemap-ref-by-sharedname filemap/2 sharedname #f))
  (when info
    (let ((id (sharedinfo-vid info)))
      (hashmap-delete! first id)
      (hashmap-delete! second sharedname))))

(define (return! stats body)
  (define callctx (callcontext/p))
  (define cont (callcontext-break callctx))
  (cont stats body))

(define* (respond #:optional body #:key
                            (status 200)
                            (title "TegFS")
                            (extra-heads '())
                            (doctype "<!DOCTYPE html>\n")
                            (content-type-params '((charset . "utf-8")))
                            (content-type 'text/html)
                            (extra-headers '()))
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define cont (callcontext-break callctx))
  (define _perm (get-permissions))
  (define key (callcontext-key callctx))
  (define key-headers
    (if key (list (web-set-cookie-header "key" key)) '()))

  (cont
   (build-response
    #:code status
    ;; most of these settings come from here: https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html
    #:headers
    (append web-basic-headers
            `((content-type . (,content-type ,@content-type-params))
              (Cache-Control . "no-cache")
              ,@key-headers
              ,@extra-headers)))
   (lambda (port)
     (parameterize ((current-output-port port))
       (when doctype (display doctype))
       (display "<html>\n")
       (display "<head>\n")
       (when title
         (display "  <title>")
         (display title)
         (display "</title>\n"))
       (display "  <link rel='stylesheet' href='/main.css'>")
       (for-each display extra-heads)
       (display "</head>\n")
       (display "<body>\n")
       (cond
        ((string? body) (display body))
        ((pair? body) (sxml->xml body port))
        ((procedure? body)
         (parameterize ((callcontext/p callctx)
                        (context/p ctx))
           (body)))
        (else (raisu 'unknown-body-type body)))
       (display "\n</body>\n")
       (display "</html>\n")))))

(define (bad-request fmt . args)
  (define str (apply stringf (cons fmt args)))
  (respond str #:status 400))

(define (not-found)
  (define request (callcontext-request (callcontext/p)))
  (respond
   (string-append "Resource not found: "
                  (uri->string (request-uri request)))
   #:status 404))

(define (main.css)
  (return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((content-type . (text/css))
              (Cache-Control . "max-age=3600, public, private"))))
   web-style))

(define (static-message message)
  (define xml (web-message-template message))
  (lambda _ (respond xml)))

(define (static-error-message status message)
  (define xml (web-message-template message))
  (lambda _
    (respond xml #:status status)))

(define (login)
  (respond web-login-body))

(define (generate-token)
  (list->string (random-choice 60 alphanum-lowercase/alphabet)))

(define body-not-found
  (static-error-message 417 "Send user body"))

(define (set-user-key! key)
  (set-callcontext-key! (callcontext/p) key))

(define (make-permission! expiery-time admin? detailsaccess?)
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define tokens (context-tokens ctx))
  (define token (generate-token))
  (define start (callcontext-time callctx))
  (define time expiery-time)
  (define perm
    (permission-constructor
     token start time admin? detailsaccess?
     (make-hashmap) (make-hashset)))
  (hashmap-set! tokens token perm)
  perm)

(define (logincont)
  (define body/bytes (callcontext-body (callcontext/p)))

  (define _4
    (unless body/bytes
      (body-not-found)))

  (define body
    (bytevector->string body/bytes "utf-8"))

  (define parts
    (string-split/simple body #\&))

  (define key-values
    (map (fn string-split/simple % #\=) parts))

  (define _2
    (unless (list-singleton? key-values)
      (raisu 'too-many-query-parameters key-values)))

  (define-tuple (key value)
    (car key-values))

  (define _3
    (unless (equal? "psw" key)
      (raisu 'bad-query-key key)))

  (define passw
    (sha256sum value))

  (define ctx (context/p))
  (define passwords (context-passwords ctx))
  (define registered? (hashmap-ref passwords passw #f))
  (define admin? #t) ;; TODO: read from the config
  (define detailsaccess? #t) ;; TODO: read from the config

  (if registered?
      (let* ((perm (make-permission! default-login-expiery-time admin? detailsaccess?))
             (token (permission-token perm)))
        (respond
         web-login-success-body
         #:extra-headers (list (web-set-cookie-header "pwdtoken" token))))
      (respond web-login-failed-body)))

(define permission-denied
  (static-error-message 401 "Permission denied"))

(define (parse-cookies-string cookies/string)
  (define _aa
    (unless (string? cookies/string)
      (raisu 'bad-cookies-cdr cookies/string)))

  (define cookie-split-semicolon
    (string-split/simple cookies/string #\;))

  (define cookie-split
    (map
     (lambda (c)
       (define-values (key eq val) (string-split-3 #\= c))
       (unless eq
         (raisu 'bad-cookie-split cookies/string))
       (cons (string-strip key) val))
     cookie-split-semicolon))

  cookie-split)

(define (get-cookie name request)
  (let* ((headers (request-headers request))
         (cookies-p (assoc 'cookie headers))
         (cookies/string (and (pair? cookies-p) (cdr cookies-p)))
         (cookies (and cookies/string (parse-cookies-string cookies/string)))
         (got (and cookies (assoc name cookies))))
    (and got (cdr got))))

(define (check-permissions)
  (unless (get-permissions)
    (permission-denied)))

(define (error-tags-list tags)
  (static-error-message 400 (string-append "Some tags are ambiguous: " (~a tags))))

(define duplicates-tags-list
  (static-error-message 400 "Tags contain duplicates"))

(define (upload-success-page <target>)
  (if <target>
      (static-error-message
       200 (string-append "Uploaded successfully to filename: " <target>))
      (static-error-message 200 "Uploaded successfully")))

(define (uploadcont)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define body/bytes (callcontext-body callctx))
  (define body/hash (parse-multipart-as-hashmap body/bytes))

  (define title
    (appcomp body/hash
             ((fn hashmap-ref % "title" #f))
             (assoc 'data)
             cdr
             ((fn bytevector->string % "utf-8"))))

  (define tags
    (appcomp body/hash
             ((fn hashmap-ref % "tags" #f))
             (assoc 'data)
             cdr
             ((fn bytevector->string % "utf-8"))))

  (define file-content
    (appcomp body/hash
             ((fn hashmap-ref % "file" #f))
             (assoc 'data)
             cdr))

  (define filename
    (appcomp body/hash
             ((fn hashmap-ref % "file" #f))
             (assoc 'Content-Disposition:filename)
             cdr))

  (define-values (<target> full-filename)
    (if (not filename) (values #f #f)
        (let* ((f1
                (append-posix-path (get-root)
                                   (dirname upload-registry-filename)
                                   filename))
               (t
                (if (file-or-directory-exists? f1)
                    (string-append (get-random-basename) "-" filename)
                    filename))
               (f2
                (append-posix-path (get-root)
                                   (dirname upload-registry-filename)
                                   t)))
          (values t f2))))

  (define _44
    (when full-filename
      (make-directories (dirname full-filename))
      (let ((port (open-file-port full-filename "w")))
        (put-bytevector port file-content)
        (close-port port))))

  (define tags-list-result
    (tegfs-process-categorization-text tags))

  ;; TODO: edit the categorization file
  (define tags-list
    (cond
     ((assoc 'ambiguous tags-list-result)
      (error-tags-list (cdr (assoc 'ambiguous tags-list-result))))
     (else
      (cdr (assoc 'ok tags-list-result)))))

  (tegfs-add
   <target> title tags-list
   #f ;; TODO: accept series? flag
   '() ;; TODO: accept key-value-pairs from the user
   upload-registry-filename #f
   )

  ((upload-success-page <target>)))

(define (upload)
  (respond (web-make-upload-body)))

(define (get-sharedinfo-for-perm perm target-fullpath)
  (define ctx (context/p))
  (define filemap/2 (context-filemap/2 ctx))
  (define perm-filemap (permission-filemap perm))
  (hashmap-ref perm-filemap target-fullpath #f))

(define (symlink-shared-file target-fullpath sharedname)
  (define ctx (context/p))
  (define sharedir (context-sharedir ctx))
  (define shared-fullpath (append-posix-path sharedir sharedname))
  (define target-fullpath/abs
    (if (absolute-posix-path? target-fullpath) target-fullpath
        (append-posix-path (get-current-directory) target-fullpath)))

  (unless (file-or-directory-exists? shared-fullpath)
    (catch-any
     (lambda _
       (symlink target-fullpath/abs shared-fullpath))
     (lambda errors
       (display "Error symlinking: ")
       (write errors)
       (newline)))))

(define (share-file/new target-fullpath for-duration make-symlink?)
  (define ctx (context/p))
  (define filemap/2 (context-filemap/2 ctx))
  (define perm (get-permissions))
  (define token (permission-token perm))
  (define info (make-sharedinfo token target-fullpath for-duration))
  (define sharedname (sharedinfo-sharedname info))
  (define perm-filemap (permission-filemap perm))

  (hashmap-set! perm-filemap target-fullpath info)
  (filemap-set! filemap/2 info)

  (when make-symlink?
    (symlink-shared-file target-fullpath sharedname))

  info)

(define (share-file/dont-link-yet target-fullpath for-duration)
  (define ctx (context/p))
  (define perm (get-permissions))
  (define make-symlink? #f)
  (or
   (get-sharedinfo-for-perm perm target-fullpath)
   (share-file/new target-fullpath for-duration make-symlink?)))

(define (share-file target-fullpath for-duration0)
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define perm (get-permissions))
  (define now (callcontext-time callctx))
  (define for-duration
    (min for-duration0
         (permission-time-left perm now)))
  (define make-symlink? #t)
  (and (< 0 for-duration)
       (or
        (get-sharedinfo-for-perm perm target-fullpath)
        (share-file/new target-fullpath for-duration make-symlink?))))

(define (display-preview target-id target-fullpath)
  (define ctx (context/p))
  (define fileserver (context-fileserver ctx))
  (define preview-fullpath (get-preview-path target-id target-fullpath))
  (define default-preview
    (if (a-weblink? target-fullpath) "/previewunknownurl" "/previewunknown"))

  (display "<img src=")
  (unless
      (and preview-fullpath
           (let ((info (share-file preview-fullpath default-preview-sharing-time)))
             (and info
                  (let* ((sharedname (sharedinfo-sharedname info))
                         (sharedir (context-sharedir ctx))
                         (shared-fullpath (append-posix-path sharedir sharedname))
                         (location (string-append fileserver sharedname)))
                    (if (file-or-directory-exists? shared-fullpath)
                        (write location)
                        (write default-preview))
                    #t))))
    (write default-preview))
  (display "/>"))

(define (local-file-entry? entry)
  (not (not (assoc entry-parent-directory-vid-key entry))))

(define (get-full-link entry target-fullpath)
  (cond
   ((a-weblink? target-fullpath)
    target-fullpath)
   ((local-file-entry? entry)
    (let* ((parent-vid (or (assoc-or entry-parent-directory-vid-key entry #f)
                           (raisu 'entry-does-not-have-parent-vid entry)))
           (ctx (context/p))
           (filemap/2 (context-filemap/2 ctx))
           (info (or (filemap-ref-by-vid filemap/2 parent-vid #f)
                     (raisu 'entry-has-bad-parent-vid entry)))
           (suffix (or (assoc-or 'target entry #f)
                       (raisu 'entry-does-not-have-target entry))))
      (if (file-is-directory?/no-readlink target-fullpath)
          (stringf "/directory?vid=~a&s=~a" parent-vid (uri-encode suffix))
          (let* ((fileserver (context-fileserver ctx))
                 (sharedname (sharedinfo-sharedname info)))
            (append-posix-path fileserver sharedname suffix)))))
   (else
    (let* ((info (share-file/dont-link-yet target-fullpath default-full-sharing-time))
           (vid (and info (sharedinfo-vid info)))
           (location (and info (string-append "/full?vid=" vid))))
      (and info location)))))

(define (maybe-display-preview entry)
  (define target-fullpath (entry-target-fullpath entry))
  (when target-fullpath
    (let* ((target-id (cdr (assoc 'id entry)))
           (full-link (get-full-link entry target-fullpath)))
      (when full-link
        (display "<a href=") (write full-link) (display ">")
        (display-preview target-id target-fullpath)
        (display "</a>")))))

(define (display-title perm entry)
  (define details-link? (has-access-for-entry-details? perm entry))

  (when details-link?
    (display "<a href='/details?id=")
    (display (cdr (assoc 'id entry)))
    (display "' style='color: white'>"))

  (cond
   ((and (assoc 'title entry)
         (not (string-null? (cdr (assoc 'title entry)))))
    (display (cdr (assoc 'title entry))))
   ((and (assoc 'target entry)
         (not (string-null? (cdr (assoc 'target entry)))))
    (display (path-get-basename (cdr (assoc 'target entry)))))
   (else
    (display (cdr (assoc 'id entry)))))

  (when details-link?
    (display "</a>"))
  )

(define (has-access-for-entry? perm entry)
  (and perm
       (or (permission-admin? perm)
           (if (local-file-entry? entry)
               (let* ((parent-vid (or (assoc-or entry-parent-directory-vid-key entry #f)
                                      (raisu 'entry-does-not-have-parent-vid entry)))
                      (ctx (context/p))
                      (filemap/2 (context-filemap/2 ctx))
                      (info (filemap-ref-by-vid filemap/2 parent-vid #f))
                      (target-fullpath (and info (sharedinfo-sourcepath info)))
                      (perm-filemap (permission-filemap perm)))
                 (and target-fullpath
                      (not (not (hashmap-ref perm-filemap target-fullpath #f)))))
               (let ((id (cdr (assoc 'id entry)))
                     (idset (permission-idset perm)))
                 (hashset-ref idset id))))))

(define (has-access-for-entry-full? perm entry)
  (has-access-for-entry? perm entry))

(define (has-access-for-entry-details? perm entry)
  (and perm
       (and (permission-detailsaccess? perm)
            (has-access-for-entry? perm entry))))

(define (display-entry entry)
  (define perm (get-permissions))
  (when (has-access-for-entry-full? perm entry)
    (display "<div class='card'>")
    (display "<div>")
    (maybe-display-preview entry)
    (display "</div>")
    (display "<div>")
    (display-title perm entry)
    (display "</div>")
    (display "</div>")
    ))

(define (decode-query query/encoded)
  (appcomp query/encoded
           web-try-uri-decode
           string->list
           (map (lambda (c) (if (equal? #\+ c) #\space c)))
           (map (lambda (c) (if (equal? #\: c) #\= c)))
           list->string))

(define (display-entries for-each-generator)
  (display "<div class='cards'>")
  (for-each-generator display-entry)
  (display "</div>"))

(define (query)
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (get-query))

  (define query/encoded (hashmap-ref ctxq 'q ""))
  (define query (decode-query query/encoded))
  (define query/split (string->words query))

  (respond
   (lambda _
     (display-entries
      (lambda (fn) (tegfs-query query/split fn))))))

(define (directory)
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define sharedir (context-sharedir ctx))
  (define filemap/2 (context-filemap/2 ctx))
  (define ctxq (get-query))
  (define root (get-root))

  (define vid
    (or (hashmap-ref ctxq 'vid #f)
        (bad-request "Request query missing requiered 'd' argument")))
  (define info
    (or (filemap-ref-by-vid filemap/2 vid #f)
        (not-found)))
  (define sharedname
    (sharedinfo-sharedname info))
  (define suffix
    (hashmap-ref ctxq 's "."))
  (define shared-link-fullpath
    (append-posix-path sharedir sharedname))
  (define shared-fullpath
    (readlink shared-link-fullpath))
  (define _1213
    (unless (string-prefix? root shared-fullpath)
      (bad-request "Bad directory")))
  (define shared-relativepath
    (remove-common-prefix shared-fullpath root))
  (define dir-fullpath
    (append-posix-path shared-fullpath suffix))
  (define dir
    (remove-common-prefix dir-fullpath root))
  (define file-names
    (let ((include-directories? #t))
      (map cadr (directory-files dir-fullpath include-directories?))))
  (define entries
    (map (comp (append-posix-path suffix)
               (standalone-file->entry/prefixed shared-relativepath vid))
         file-names))

  (respond
   (lambda _
     (display-entries
      (lambda (fn) (for-each fn entries))))))

(define (web-make-preview target-id target-fullpath entry)
  (define preview-fullpath
    (get-preview-path target-id target-fullpath))

  (and (or (file-or-directory-exists? preview-fullpath)
           (catch-any
            (lambda _ (tegfs-make-thumbnails target-fullpath preview-fullpath))
            (lambda _ #f)))
       preview-fullpath))

(define unavailable-image-string
  (stringf
   "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
    <!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>
    <svg width='~apx' height='~apx' viewBox='0 0 24 24' fill='none' xmlns='http://www.w3.org/2000/svg'><path fill-rule='evenodd' clip-rule='evenodd' d='M18.364 5.63604C21.8787 9.15076 21.8787 14.8492 18.364 18.364C14.8492 21.8787 9.15076 21.8787 5.63604 18.364C2.12132 14.8492 2.12132 9.15076 5.63604 5.63604C9.15076 2.12132 14.8492 2.12132 18.364 5.63604ZM16.1925 17.6067L6.39327 7.80749C4.33767 10.5493 4.55666 14.4562 7.05025 16.9497C9.54384 19.4433 13.4507 19.6623 16.1925 17.6067ZM16.9497 7.05025C19.4433 9.54384 19.6623 13.4507 17.6067 16.1925L7.80749 6.39327C10.5493 4.33767 14.4562 4.55666 16.9497 7.05025Z' fill='black'/></svg>"
   web-preview-width web-preview-height
   ))

(define unavailable-bytevector
  (string->utf8 unavailable-image-string))

(define unavailable-response
  (build-response
   #:code 200
   #:headers
   (append web-basic-headers
           `((content-type . (image/svg+xml))
             (Cache-Control . "max-age=3600, public, private")))))

(define (previewunknown)
  (return! unavailable-response unavailable-bytevector))

(define (preview-unknownurl)
  (return! unknownurl-response unknownurl-bytevector))

(define unknownurl-bytevector
  (string->utf8 (web-url-icon/svg web-preview-width web-preview-height)))

(define unknownurl-response
  (build-response
   #:code 200
   #:headers
   (append web-basic-headers
           `((content-type . (image/svg+xml))
             (Cache-Control . "max-age=3600, public, private")))))

(define (previewunknownurl)
  (return! unknownurl-response unknownurl-bytevector))

(define (preview)
  (define ctxq (get-query))
  (define target-id (hashmap-ref ctxq 't #f))
  (define entry
    (or (tegfs-get/cached target-id)
        (not-found)))
  (define target-fullpath (entry-target-fullpath entry))
  (define preview-fullpath
    (web-make-preview target-id target-fullpath entry))

  (if preview-fullpath
      (web-sendfile return! 'image/jpeg preview-fullpath)
      (previewunknown)))

(define (full)
  (define ctx (context/p))
  (define filemap/2 (context-filemap/2 ctx))
  (define perm (get-permissions))
  (define _87123
    (unless perm
      ;; not logged in
      (not-found)))
  (define ctxq (get-query))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define info (filemap-ref-by-vid filemap/2 vid #f))
  (define _8123
    (unless info
      (not-found)))
  (define sharedname (sharedinfo-sharedname info))
  (define target-fullpath (sharedinfo-sourcepath info))
  (define _11
    (unless (hashmap-ref (permission-filemap perm) target-fullpath #f)
      ;; file was not shared with this permission
      (not-found)))
  (define fileserver (context-fileserver ctx))
  (define location
    (if (file-is-directory?/no-readlink target-fullpath)
        (string-append "/directory?vid=" vid)
        (string-append fileserver sharedname)))

  (symlink-shared-file target-fullpath sharedname)

  (return!
   (build-response
    #:code 301
    #:headers
    (append web-basic-headers
            `((Location . ,location)
              (Cache-Control . "no-cache"))))
   #f))

(define permission-time-left
  (case-lambda
   ((perm)
    (permission-time-left perm (time-get-current-unixtime)))
   ((perm current-time)
    (define end (+ (permission-start perm)
                   (permission-time perm)))
    (max 0 (- end current-time)))))

(define permission-still-valid?
  (case-lambda
   ((perm)
    (permission-still-valid? perm (time-get-current-unixtime)))
   ((perm current-time)
    (< 0 (permission-time-left perm current-time)))))

(define (invalidate-permission perm)
  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define token (permission-token perm))
  (hashmap-delete! tokens token)
  (values))

(define sharedinfo-time-left
  (case-lambda
   ((info)
    (sharedinfo-time-left info (time-get-current-unixtime)))
   ((info current-time)
    (define end (+ (sharedinfo-ctime info)
                   (sharedinfo-stime info)))
    (max 0 (- end current-time)))))

(define sharedinfo-still-valid?
  (case-lambda
   ((info)
    (sharedinfo-still-valid? info (time-get-current-unixtime)))
   ((info current-time)
    (< 0 (sharedinfo-time-left info current-time)))))

(define (collectgarbage)
  (define callctx (callcontext/p))
  (define now (callcontext-time callctx))

  (collectgarbage/nocall now)

  (return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((Cache-Control . "no-cache"))))
   "ok\n"))

(define (collectgarbage/nocall now)
  (define ctx (context/p))
  (define sharedir (context-sharedir ctx))
  (define filemap/2 (context-filemap/2 ctx))
  (define tokens (context-tokens ctx))
  (define delayed-list '())
  (define-syntax delayop
    (syntax-rules ()
      ((_ . bodies)
       (set! delayed-list
             (cons (lambda _ . bodies) delayed-list)))))

  (hashmap-foreach
   (lambda (sharedname info)
     (unless (sharedinfo-still-valid? info)
       (delayop
        (display "UNSHARE ") (write sharedname) (newline)
        (filemap-delete-by-sharedname! filemap/2 sharedname))))
   (cdr filemap/2))

  (hashmap-foreach
   (lambda (token perm)
     (if (permission-still-valid? perm now)
         (hashmap-foreach
          (lambda (target-fullpath info)
            (unless (sharedinfo-still-valid? info)
              (delayop
               (display "UNPERM ")
               (write target-fullpath) (newline)
               (hashmap-delete!
                (permission-filemap perm) target-fullpath))))
          (permission-filemap perm))
         (delayop
          (hashmap-delete! tokens token))))
   tokens)

  (for-each (lambda (delayed) (delayed)) delayed-list)

  (for-each
   (lambda (namepair)
     (define full-name (car namepair))
     (define sharedname (cadr namepair))
     (define info (filemap-ref-by-sharedname filemap/2 sharedname #f))
     (unless info
       (display "File not shared: ")
       (write sharedname)
       (display " deleting...\n")
       (file-delete full-name)))
   (directory-files sharedir)))

(define (share)
  (define ctx (context/p))
  (define ctxq (get-query))
  (define callctx (callcontext/p))
  (define req (callcontext-request callctx))

  (define query/encoded (hashmap-ref ctxq 'q ""))
  (define query (decode-query query/encoded))
  (define query/split (string->words query))

  (define admin? #f)
  (define detailsaccess? #f) ;; TODO: maybe allow sometimes
  (define perm (make-permission! default-share-expiery-time admin? detailsaccess?))
  (define idset (permission-idset perm))
  (define token (permission-token perm))
  (define location
    (stringf "/query?q=~a&key=~a" query/encoded token))
  (define hidden-query-location
    (stringf "/query?q=%any&key=~a" token))

  (define domainname
    (web-request-get-domainname req))
  (define (print-link url0)
    (define url (string-append domainname url0))
    (sxml->xml `(a (@ (href ,url)) ,url)))
  (define (print-newline)
    (display "<br>\n"))
  (define (print-line title url)
    (display title)
    (display ":")
    (print-newline)
    (print-link url)
    (print-newline))
  (define text
    (with-output-to-string
      (lambda _
        (parameterize ((current-error-port (current-output-port)))
          (print-line "Default link" location)
          (print-line "Hidden query link" hidden-query-location)
          (print-newline) (print-newline)
          (display "Second then forth:")
          (print-newline)
          (print-link
           (stringf "/query?q=ll&key=~a" (generate-token)))
          (print-newline)
          (print-link
           (stringf "/query?q=ll&key=~a" token))
          (print-newline)
          (print-link
           (stringf "/query?q=ll&key=~a" (generate-token)))
          (print-newline)
          (print-link "/query?q=%any")
          (print-newline)
          (print-link
           (stringf "/query?q=ll&key=~a" (generate-token)))))))

  (tegfs-query
   query/split
   (lambda (entry)
     (define id (cdr (assoc 'id entry)))
     (hashset-add! idset id)))

  (respond text))

(define (details)
  (define ctx (context/p))
  (define ctxq (get-query))
  (define id (hashmap-ref ctxq 'id #f))
  (define perm (get-permissions))
  (define entry
    (or (tegfs-get/cached id)
        (not-found)))
  (define table
    (with-output-to-string
      (lambda _
        (display "<table class='styled-table subc'>\n")
        (display "  <thead><tr><th>Prop</th><th>Value</th></tr></thead>\n")
        (for-each
         (lambda (row)
           (define name (car row))
           (define val (cdr row))
           (display "  <tbody>\n")
           (display "    <tr>\n")
           (display "      <td>")
           (display name)
           (display "</td>\n")
           (display "      <td>")
           (display val)
           (display "</td>\n")
           (display "    </tr>\n")
           (display "  </tbody>\n"))
         entry)
        (display "</table>\n"))))

  (unless (has-access-for-entry-details? perm entry)
    (not-found))

  (respond table))

(define handlers-config
  `((/login ,login public)
    (/logincont ,logincont public)
    (/main.css ,main.css public)
    (/collectgarbage ,collectgarbage public)
    (/query ,query public)
    (/directory ,directory public)
    (/details ,details public)
    (/full ,full public)
    (/upload ,upload)
    (/uploadcont ,uploadcont)
    (/preview ,preview)
    (/previewunknown ,previewunknown)
    (/previewunknownurl ,previewunknownurl)
    (/share ,share)
    ))

(define handlers-funcmap
  (alist->hashmap
   (map
    (lambda (p) (cons (~a (car p)) (cadr p)))
    handlers-config)))

(define handlers-publicset
  (list->hashset
   (map
    (comp car ~a)
    (filter
     (lambda (l) (and (= 3 (length l))
                      (equal? 'public (caddr l))))
     handlers-config))))

(define (log-request path request)
  (display "Got request: ") (display path)
  (display "\n")
  )

(define (handler request body)
  (define path (uri-path (request-uri request)))

  (log-request path request)

  (let* ((target path)
         (func (hashmap-ref handlers-funcmap target #f))
         (public? (hashset-ref handlers-publicset target)))
    (unless func (not-found))
    (unless public? (check-permissions))
    (func)))

(define (make-context)
  (define passwords (make-hashmap))
  (define database (make-hashmap))
  (define tokens (make-hashmap))

  (define filemap/2
    (cons (make-hashmap) (make-hashmap)))
  (define config
    (get-config))
  (define _1
    (unless config
      (raisu 'no-config-file
             "Config file needs to be present when starting the server")))
  (define users
    (cdr
     (or (assoc 'users config)
         (raisu 'no-config-file
                "Config file needs to be present when starting the server"))))
  (define fileserver
    (cadr
     (or (assoc 'fileserver config)
         (raisu 'no-fileserver "Variable 'fileserver is not set by the config"))))
  (define sharedir
    (cadr
     (or (assoc 'sharedir config)
         (raisu 'no-fileserver "Variable 'sharedir is not set by the config"))))
  (define port/string
    (cadr
     (or (assoc 'port config)
         (raisu 'no-port "Variable 'port is not set by the config"))))
  (define port
    (or (string->number (~a port/string))
        (raisu 'port-is-not-a-number "Variable 'port must be a number" port/string)))

  (unless (file-or-directory-exists? sharedir)
    (make-directories sharedir))

  (for-each
   (lambda (user)
     (define pass
       (cadr
        (or (assoc 'pass user)
            (raisu 'no-user-pass
                   "A user does not have a password"))))
     (unless (string? pass)
       (raisu 'pass-is-no-string "User passord is not a string" pass))
     (hashmap-set! passwords pass #t))
   users)

  (unless (string? fileserver)
    (raisu 'fileserver-must-be-a-string fileserver))
  (unless (string? sharedir)
    (raisu 'sharedir-must-be-a-string sharedir))
  (unless (and (integer? port) (exact? port) (> port 0))
    (raisu 'port-must-be-a-natural-number port))

  (context-ctr passwords database tokens port fileserver sharedir filemap/2))

(define (query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define-values (key eq val) (string-split-3 #\= sp))
           (when (string-null? eq) (raisu 'bad-query query sp))
           (cons (string->symbol key) (web-try-uri-decode val)))
         split))
  (alist->hashmap key-values))

(define (get-access-token)
  (or
   (let* ((qH (get-query))
          (ret (hashmap-ref qH 'key #f)))
     (when ret (set-user-key! ret))
     ret)
   (let* ((callctx (callcontext/p))
          (request (callcontext-request callctx)))
     (or (get-cookie "key" request)
         (get-cookie "pwdtoken" request)))))

(define (initialize-permissions)
  (define token (get-access-token))
  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define existing (hashmap-ref tokens token #f))

  (and existing
       (if (permission-still-valid? existing) existing
           (begin
             (invalidate-permission existing)
             #f))))

(define (get-permissions)
  (define callctx (callcontext/p))
  (define f (callcontext-permissions callctx))
  (f))

(define (get-query)
  (define callctx (callcontext/p))
  (define f (callcontext-query callctx))
  (f))

(define (initialize-query request)
  (define uri (request-uri request))
  (define query/encoded (uri-query uri))
  (if query/encoded
      (query->hashmap query/encoded)
      (make-hashmap)))

(define (make-callcontext break request body)
  (define qH (memconst (initialize-query request)))
  (define perm (memconst (initialize-permissions)))
  (define time (time-get-current-unixtime))
  (callcontext-ctr break request qH body time #f perm))

(define (make-handler)
  (lambda (request body)
    (call-with-current-continuation
     (lambda (k)
       (parameterize ((callcontext/p (make-callcontext k request body)))
         (handler request body))))))

(define (tegfs-serve/parse)
  (dprintln "Starting the server")
  (parameterize ((context/p (make-context)))
    (let ((port (context-port (context/p))))

      (dprintln "Collecting garbage left from the previous run...")
      (collectgarbage/nocall (time-get-current-unixtime))
      (dprintln "Done")

      (run-server (make-handler) 'http `(#:port ,port)))))
