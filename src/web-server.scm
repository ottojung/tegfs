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
%use (path-extension) "./euphrates/path-extension.scm"
%use (path-extensions) "./euphrates/path-extensions.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (get-current-directory) "./euphrates/get-current-directory.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (directory-files) "./euphrates/directory-files.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (memconst) "./euphrates/memconst.scm"

%use (root/p) "./root-p.scm"
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
%use (get-file-type) "./get-file-type.scm"
%use (read-file-head) "./read-file-head.scm"
%use (get-config) "./get-config.scm"

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

(define-type9 <context>
  (context-ctr passwords database tokens port fileserver sharedir filemap) context?
  (passwords context-passwords) ;; user credentials passwords
  (database context-database) ;; tag database
  (tokens context-tokens) ;; temporary session tokens
  (port context-port) ;; port to host the server on
  (fileserver context-fileserver) ;; full URI of the file server
  (sharedir context-sharedir) ;; directory with shared wiles
  (filemap context-filemap) ;; hashmap of type [: id -> shared filepath]
  )

(define-type9 <callcontext>
  (callcontext-ctr break request query body key permissions) callcontext?
  (break callcontext-break) ;; break handler
  (request callcontext-request) ;; client request
  (query callcontext-query) ;; query hashmap
  (body callcontext-body) ;; client body
  (key callcontext-key set-callcontext-key!) ;; access key to-set to
  (permissions callcontext-permissions) ;; permissions associated with this call
  )

(define-type9 <sharedinfo>
  (sharedinfo-ctr token sourcepath targetpath ctime stime) sharedinfo?
  (token sharedinfo-token) ;; token of the perms that shared this file
  (sourcepath sharedinfo-sourcepath) ;; the original file path
  (targetpath sharedinfo-targetpath) ;; the linked file path suffix (without the sharedir)
  (id sharedinfo-id) ;; mapped file id
  (ctime sharedinfo-ctime) ;; time in seconds for when this info was created
  (stime sharedinfo-stime) ;; time in seconds for how long to share this file
  )

(define-type9 <permission>
  (permission-constructor token start time admin? filemap idset) permission?
  (token permission-token) ;; token string
  (start permission-start) ;; timestamp for when this token was created
  (time permission-time) ;; duration in secods for how long this token is valid
  (admin? permission-admin?) ;; true if user is an admin
  (filemap permission-filemap) ;; hashmap with `keys: target-fullpath`, `values: file info that is shared for this permissions`
  (idset permission-idset) ;; hashset with `values: id of entry that is shared with this permission`
  )

(define context/p
  (make-parameter #f))

(define callcontext/p
  (make-parameter #f))

(define default-sharing-time
  (string->seconds "1h"))
(define default-preview-sharing-time
  (string->seconds "30m"))
(define default-full-sharing-time
  (string->seconds "30m"))
(define default-login-expiery-time
  (string->seconds "12h"))
(define default-share-expiery-time
  (string->seconds "1h"))

(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (return! stats body)
  (define callctx (callcontext/p))
  (define cont (callcontext-break callctx))
  (cont stats body))

(define* (respond #:optional body #:key
                            (status 200)
                            (title #f)
                            (extra-heads '())
                            (doctype "<!DOCTYPE html>\n")
                            (content-type-params '((charset . "utf-8")))
                            (content-type 'text/html)
                            (extra-headers '()))
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
        ((procedure? body) (body))
        (else (raisu 'unknown-body-type body)))
       (display "\n</body>\n")
       (display "</html>\n")))))

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

(define (make-permission! expiery-time admin?)
  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define token (generate-token))
  (define start (time-get-current-unixtime))
  (define time expiery-time)
  (define perm
    (permission-constructor
     token start time admin?
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

  (if registered?
      (let* ((perm (make-permission! default-login-expiery-time admin?))
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

  (define extension
    (path-extension filename))

  (define <target>
    (and (not (string-null? filename))
         (append-posix-path (string-append (get-random-basename) extension))))

  (define full-filename
    (and <target>
         (append-posix-path (root/p)
                            (dirname upload-registry-filename)
                            <target>)))

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

(define (get-preview-by-id target-id target-fullpath)
  (define preview-directory
    (append-posix-path (root/p) "cache" "preview"))
  (define file-type (get-file-type target-fullpath))
  (define preview-name
    (string-append
     target-id
     (case file-type
      ((image) ".jpeg")
      ((video) ".gif")
      (else "TODO???"))))
  (define preview-fullpath
    (append-posix-path preview-directory preview-name))

  (unless (file-or-directory-exists? preview-directory)
    (make-directories preview-directory))

  preview-fullpath)

(define (get-sharedinfo-for-perm perm target-fullpath)
  (define ctx (context/p))
  (define filemap (context-filemap ctx))
  (define perm-filemap (permission-filemap perm))
  (define info (hashmap-ref perm-filemap target-fullpath #f))
  (and info
       (let ((shared-name (sharedinfo-targetpath info)))
         (hashmap-ref filemap shared-name #f))))

(define (symlink-shared-file target-fullpath shared-name)
  (define ctx (context/p))
  (define sharedir (context-sharedir ctx))
  (define shared-fullpath (append-posix-path sharedir shared-name))
  (define target-fullpath/abs
    (if (absolute-posix-path? target-fullpath) target-fullpath
        (append-posix-path (get-current-directory) target-fullpath)))

  (unless (file-or-directory-exists? shared-fullpath)
    (symlink target-fullpath/abs shared-fullpath)))

(define (share-file/new target-fullpath for-duration make-symlink?)
  (define ctx (context/p))
  (define filemap (context-filemap ctx))
  (define perm (get-permissions))
  (define shared-name
    (string-append (get-random-basename)
                   (path-extensions target-fullpath)))
  (define now (time-get-current-unixtime))
  (define token (permission-token perm))
  (define info (sharedinfo-ctr token target-fullpath shared-name now for-duration))
  (define perm-filemap (permission-filemap perm))

  (hashmap-set! perm-filemap target-fullpath info)
  (hashmap-set! filemap shared-name info)

  (when make-symlink?
    (symlink-shared-file target-fullpath shared-name))

  info)

(define (share-file/dont-link-yet target-fullpath for-duration)
  (define ctx (context/p))
  (define filemap (context-filemap ctx))
  (define perm (get-permissions))
  (define make-symlink? #f)
  (or
   (get-sharedinfo-for-perm perm target-fullpath)
   (share-file/new target-fullpath for-duration make-symlink?)))

(define (share-file target-fullpath for-duration)
  (define ctx (context/p))
  (define filemap (context-filemap ctx))
  (define perm (get-permissions))
  (define make-symlink? #t)
  (or
   (get-sharedinfo-for-perm perm target-fullpath)
   (share-file/new target-fullpath for-duration make-symlink?)))

(define (display-preview target-id target-fullpath)
  (define ctx (context/p))
  (define fileserver (context-fileserver ctx))
  (define preview-fullpath (get-preview-by-id target-id target-fullpath))
  (define perm (get-permissions))
  (define info (share-file preview-fullpath default-preview-sharing-time))

  (display "<img src=")
  (if info
      (let* ((shared-name (sharedinfo-targetpath info))
             (sharedir (context-sharedir ctx))
             (shared-fullpath (append-posix-path sharedir shared-name))
             (location (string-append fileserver shared-name)))
        (if (file-or-directory-exists? shared-fullpath)
            (write location)
            (write "/previewuknown")))
      (write "/previewuknown"))
  (display "/>"))

(define (display-full-link entry target-fullpath)
  (define id (cdr (assoc 'id entry)))
  (define location (string-append "/full?id=" id))
  (share-file/dont-link-yet target-fullpath default-full-sharing-time)
  (display location))

(define (maybe-display-preview entry)
  (define target-fullpath (entry-target-fullpath entry))
  (when target-fullpath
    (let* ((target-id (cdr (assoc 'id entry))))
      (display "<a href='") (display-full-link entry target-fullpath) (display "'>")
      (display-preview target-id target-fullpath)
      (display "</a>")
      )))

(define (display-title entry)
  (display "<a href='/info/")
  (display (cdr (assoc 'id entry)))
  (display "' style='color: white'>")
  (cond
   ((and (assoc 'title entry)
         (not (string-null? (cdr (assoc 'title entry)))))
    (display (cdr (assoc 'title entry))))
   ((and (assoc 'target entry)
         (not (string-null? (cdr (assoc 'target entry)))))
    (display (cdr (assoc 'target entry))))
   (else
    (display (cdr (assoc 'id entry)))))
  (display "</a>")
  )

(define (has-access-for-entry? perm entry)
  (and perm
       (let ((id (cdr (assoc 'id entry)))
             (idset (permission-idset perm)))
         (or (permission-admin? perm)
             (hashset-ref idset id)))))

(define (display-entry entry)
  (define perm (get-permissions))
  (when (has-access-for-entry? perm entry)
    (display "<div class='card'>")
    (display "<div>")
    (maybe-display-preview entry)
    (display "</div>")
    (display "<div>")
    (display-title entry)
    (display "</div>")
    (display "</div>")
    ))

(define (decode-query query/encoded)
  (appcomp query/encoded
           uri-decode
           string->list
           (map (lambda (c) (if (equal? #\+ c) #\space c)))
           (map (lambda (c) (if (equal? #\: c) #\= c)))
           list->string))

(define (query)
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (callcontext-query callctx))

  (define query/encoded (hashmap-ref ctxq 'q ""))
  (define query (decode-query query/encoded))
  (define query/split (string->words query))
  (define entries (tegfs-query query/split))

  (respond
   (lambda _
     (parameterize ((context/p ctx)
                    (callcontext/p callctx))
       (display "<div class='cards'>")
       (for-each display-entry  entries)
       (display "</div>")
       ))))

(define (web-make-preview target-id target-fullpath entry)
  (define preview-fullpath
    (get-preview-by-id target-id target-fullpath))

  (and (or (file-or-directory-exists? preview-fullpath)
           (tegfs-make-thumbnails target-fullpath preview-fullpath))
       preview-fullpath))

(define (entry-target-fullpath entry)
  (define target-p (assoc 'target entry))
  (and target-p
       (let* ((registry-dir (dirname (cdr (assoc entry-registry-path-key entry)))))
         (append-posix-path (root/p) registry-dir (cdr target-p)))))

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

(define (preview-unavailable)
  (return! unavailable-response unavailable-bytevector))

(define (preview)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (callcontext-query callctx))
  (define target-id (hashmap-ref ctxq 't #f))
  (define entry (tegfs-get/cached target-id))
  (define target-fullpath (entry-target-fullpath entry))
  (define preview-fullpath
    (web-make-preview target-id target-fullpath entry))

  (if preview-fullpath
      (web-sendfile return! 'image/jpeg preview-fullpath)
      (preview-unavailable)))

(define (previewuknown)
  (preview-unavailable))

(define (full)
  (define ctx (context/p))
  (define perm (get-permissions))
  (define callctx (callcontext/p))
  (define ctxq (callcontext-query callctx))
  (define id (hashmap-ref ctxq 'id #f))
  (define entry (tegfs-get/cached id))

  (define _11
    (unless (has-access-for-entry? perm entry)
      (not-found)))

  (define target-fullpath (entry-target-fullpath entry))
  (define info
    (or (get-sharedinfo-for-perm perm target-fullpath)
        (not-found)))
  (define shared-name (sharedinfo-targetpath info))
  (define fileserver (context-fileserver ctx))
  (define location (string-append fileserver shared-name))

  (symlink-shared-file target-fullpath shared-name)

  (return!
   (build-response
    #:code 301
    #:headers
    (append web-basic-headers
            `((Location . ,location)
              (Cache-Control . "no-cache"))))
   #f))

(define permission-still-valid?
  (case-lambda
   ((perm)
    (permission-still-valid? perm (time-get-current-unixtime)))
   ((perm current-time)
    (define end (+ (permission-start perm)
                   (permission-time perm)))
    (< current-time end))))

(define (invalidate-permission perm)
  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define token (permission-token perm))
  (hashmap-delete! tokens token)
  (values))

(define (collectgarbage)
  (define ctx (context/p))
  (define sharedir (context-sharedir ctx))
  (define filemap (context-filemap ctx))
  (define files (directory-files sharedir))
  (define now (time-get-current-unixtime))
  (define tokens (context-tokens ctx))

  (for-each
   (lambda (namepair)
     (define full-name (car namepair))
     (define shared-name (cadr namepair))
     (define info (hashmap-ref filemap shared-name #f))
     (if info
         (let* ((end (+ (sharedinfo-ctime info)
                        (sharedinfo-stime info))))
           (unless (< now end)
             (file-delete full-name)
             (hashmap-delete! filemap shared-name)))
         (begin
           (display "File not shared: ")
           (write shared-name)
           (display " deleting...\n")
           (file-delete full-name))))
   files)

  (hashmap-foreach
   (lambda (token perm)
     (unless (permission-still-valid? perm)
       (invalidate-permission perm)))
   tokens)

  (return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((Cache-Control . "no-cache"))))
   "ok"))

(define (share)
  (define ctx (context/p))
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (callcontext-query callctx))

  (define query/encoded (hashmap-ref ctxq 'q ""))
  (define query (decode-query query/encoded))
  (define query/split (string->words query))
  (define entries (tegfs-query query/split))

  (define admin? #f)
  (define perm (make-permission! default-share-expiery-time admin?))
  (define idset (permission-idset perm))
  (define token (permission-token perm))
  (define location (stringf "/query?q=~a&key=~a" query/encoded token))

  (for-each
   (lambda (entry)
     (define id (cdr (assoc 'id entry)))
     (hashset-add! idset id))
   entries)

  (return!
   (build-response
    #:code 301
    #:headers
    (append web-basic-headers
            `((Location . ,location)
              (Cache-Control . "no-cache"))))
   #f))

(define handlers-config
  `((/login ,login public)
    (/logincont ,logincont public)
    (/main.css ,main.css public)
    (/collectgarbage ,collectgarbage public)
    (/query ,query public)
    (/upload ,upload)
    (/uploadcont ,uploadcont)
    (/preview ,preview)
    (/previewuknown ,previewuknown)
    (/full ,full)
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

  (define filemap
    (make-hashmap))
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

  (context-ctr passwords database tokens port fileserver sharedir filemap))

(define (query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define-values (key eq val) (string-split-3 #\= sp))
           (when (string-null? eq) (raisu 'bad-query query))
           (cons (string->symbol key) val))
         split))
  (alist->hashmap key-values))

(define (get-access-token)
  (define callctx (callcontext/p))
  (or
   (let* ((qH (callcontext-query callctx))
          (ret (hashmap-ref qH 'key #f)))
     (when ret (set-user-key! ret))
     ret)
   (let ((request (callcontext-request callctx)))
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

(define (make-callcontext break request body)
  (define uri (request-uri request))
  (define query/encoded (uri-query uri))
  (define qH (if query/encoded (query->hashmap query/encoded) (make-hashmap)))
  (define perm (memconst (initialize-permissions)))
  (callcontext-ctr break request qH body #f perm))

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
      (run-server (make-handler) 'http `(#:port ,port)))))
