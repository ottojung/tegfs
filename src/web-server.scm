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
%use (make-hashmap hashmap-ref hashmap-set! hashmap->alist alist->hashmap) "./euphrates/ihashmap.scm"
%use (list->hashset hashset-ref) "./euphrates/ihashset.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (random-choice) "./euphrates/random-choice.scm"
%use (alphanum-lowercase/alphabet) "./euphrates/alphanum-lowercase-alphabet.scm"
%use (printf) "./euphrates/printf.scm"
%use (path-extension) "./euphrates/path-extension.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"

%use (root/p) "./root-p.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (tegfs-process-categorization-text) "./edit-tags.scm"
%use (tegfs-add) "./add.scm"
%use (tegfs-get) "./get.scm"
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
%use (file-is-image?) "./file-is-image-q.scm"
%use (web-sendfile) "./web-sendfile.scm"

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
  (context-ctr passwords database tokens) context?
  (passwords context-passwords) ;; user credentials passwords
  (database context-database) ;; tag database
  (tokens context-tokens) ;; temporary session tokens
  )

(define-type9 <callcontext>
  (callcontext-ctr break request query body atoken) callcontext?
  (break callcontext-break) ;; break handler
  (request callcontext-request) ;; client request
  (query callcontext-query) ;; query hashmap
  (body callcontext-body) ;; client body
  (atoken callcontext-atoken set-callcontext-atoken!) ;; access token to-set to
  )

(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

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
  (define atoken (callcontext-atoken callctx))
  (define atoken-headers
    (if atoken (list (web-set-cookie-header "atoken" atoken)) '()))

  (cont
   (build-response
    #:code status
    ;; most of these settings come from here: https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html
    #:headers
    (append web-basic-headers
            `((content-type . (,content-type ,@content-type-params))
              (Cache-Control . "no-cache")
              ,@atoken-headers
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
       (if (string? body)
           (display body)
           (sxml->xml body port))
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

(define (generate-atoken)
  (list->string (random-choice 60 alphanum-lowercase/alphabet)))

(define body-not-found
  (static-error-message 417 "Send user body"))

(define (login-user! atoken)
  (set-callcontext-atoken! (callcontext/p) atoken))

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
  (define tokens (context-tokens ctx))
  (define registered? (hashmap-ref passwords passw #f))
  (define atoken (and registered? (generate-atoken)))

  (when registered?
    (hashmap-set! tokens atoken #t))

  (login-user! atoken)

  (if registered?
      (respond web-login-success-body)
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

(define (check-permissions)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define qH (callcontext-query callctx))

  (define atoken
    (or
     (let* ((ret (hashmap-ref qH 'key #f)))
       (when ret (login-user! ret))
       ret)
     (let* ((headers (request-headers request))
            (cookies-p (assoc 'cookie headers))
            (do (unless (pair? cookies-p)
                  (permission-denied)))
            (cookies/string (cdr cookies-p))
            (cookies (parse-cookies-string cookies/string))
            (got (assoc "atoken" cookies)))
      (and got (cdr got)))))

  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define existing (hashmap-ref tokens atoken #f))

  (unless existing
    (permission-denied))

  (values))

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
  (define headers (request-headers request))
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

(define (entry)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define path (request-path-components request))

  (define _11
    (unless (= 2 (length path))
      (not-found)))

  (define id (cadr path))

  (define entry
    (tegfs-get id))

  (return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((content-type . (text/plain))
              (Cache-Control . "no-cache"))))
   (~s entry)))

(define web-preview-width 400)
(define web-preview-height 225)

(define (get-preview-by-id target-id)
  (define preview-directory
    (append-posix-path (root/p) "cache" "preview"))
  (define preview-name
    (string-append target-id ".jpeg"))
  (define preview-fullpath
    (append-posix-path preview-directory preview-name))

  (unless (file-or-directory-exists? preview-directory)
    (make-directories preview-directory))

  preview-fullpath)

(define (generate-preview-internet-path target-id)
  (string-append "/preview?t=" target-id))

(define (web-make-image-preview target-id target-fullpath)
  (define preview-fullpath
    (get-preview-by-id target-id))

  (unless (file-or-directory-exists? preview-fullpath)
    ;; TODO: check exit code
    (system-fmt
     (string-append
      "convert "
      " ~a "
      " -thumbnail ~a@ "
      " -quality 10 "
      " -gravity center "
      " -background transparent "
      " -extent ~ax~a "
      " ~a "
      )
     target-fullpath
     (* web-preview-width web-preview-height)
     web-preview-width web-preview-height
     preview-fullpath))

  preview-fullpath)

(define (web-generate-preview target-id target-fullpath)
  (cond
   ((file-is-image? target-fullpath)
    (generate-preview-internet-path target-id))
   (else
    'TODO)))

(define (display-preview preview-fullpath)
  (display "<img src=")
  (write preview-fullpath)
  (display "/>")
  )

(define (maybe-display-preview entry)
  (define target-p (assoc 'target entry))
  (when target-p
    (let* ((registry-dir (dirname (cdr (assoc 'registry-path entry))))
           (target-fullpath (append-posix-path registry-dir (cdr target-p)))
           (target-id (cdr (assoc 'id entry))))
      (display-preview
       (web-generate-preview target-id target-fullpath)))))

(define (display-title entry)
  (display "<a href='/info/")
  (display (cdr (assoc 'id entry)))
  (display "'>")
  (display "<button>")
  (cond
   ((and (assoc 'title entry)
         (not (string-null? (cdr (assoc 'title entry)))))
    (display (cdr (assoc 'title entry))))
   ((and (assoc 'target entry)
         (not (string-null? (cdr (assoc 'target entry)))))
    (display (cdr (assoc 'target entry))))
   (else
    (display (cdr (assoc 'id entry)))))
  (display "</button>")
  (display "</a>")
  )

(define (display-entry entry)
  (display "<div class='card'>")
  (display "<div>")
  (maybe-display-preview entry)
  (display "</div>")
  (display "<div>")
  (display-title entry)
  (display "</div>")
  (display "</div>")
  )

(define (query)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (callcontext-query callctx))
  (define path (request-path-components request))

  (define query (hashmap-ref ctxq 'q ""))
  (define query/split (string->words query))
  (define entries (tegfs-query query/split))

  (define str
    (with-output-to-string
      (lambda _
        (display "<div class='cards'>")
        (for-each display-entry entries)
        (display "</div>")
        )))

  (respond str))

(define (preview)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define ctxq (callcontext-query callctx))
  (define path (request-path-components request))

  (define target-id (hashmap-ref ctxq 't #f))
  (define entry (tegfs-get target-id))
  (define target-p (assoc 'target entry))
  (define target-fullpath
    (and target-p
         (let* ((registry-dir (dirname (cdr (assoc 'registry-path entry)))))
           (append-posix-path registry-dir (cdr target-p)))))

  (define preview-fullpath
    (cond
     ((file-is-image? target-fullpath)
      (web-make-image-preview target-id target-fullpath))
     (else
      'TODO)))

  (web-sendfile return! 'image/jpeg preview-fullpath))

(define (cookie1)
  (define request (callcontext-request (callcontext/p)))
  (respond
   `(p (b "Hi!") (br) "there. Your headers are the following:"
       ,(~s (request-headers request)))
   #:extra-headers `(,(web-set-cookie-header "hi" "there"))))

(define (hacker)
  (values '((content-type . (text/plain))) "Hello hacker!"))

(define handlers-config
  `((login ,login public)
    (logincont ,logincont public)
    (main.css ,main.css public)
    (hacker ,hacker public)
    (upload ,upload)
    (uploadcont ,uploadcont)
    (entry ,entry)
    (query ,query)
    (preview ,preview)))

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

(define (handler request body)
  (define path-components
    (request-path-components request))

  (dprintln "Got request: ~s" path-components)

  (unless (list-singleton? path-components)
    (not-found))

  (let* ((root (car path-components))
         (func (hashmap-ref handlers-funcmap root #f))
         (public? (hashset-ref handlers-publicset root)))
    (unless func (not-found))
    (unless public? (check-permissions))
    (func)))

(define context/p
  (make-parameter #f))

(define (make-context)
  (define passwords (make-hashmap))
  (define database (make-hashmap))
  (define tokens (make-hashmap))

  (define auth-file
    (append-posix-path (root/p) "auth.tegfs.lisp"))
  (define _1
    (unless (file-or-directory-exists? auth-file)
      (raisu 'no-auth-file
             "Auth file needs to be present when starting the server")))
  (define auth-lines
    (appcomp auth-file
             ((fn open-file-port % "r"))
             read-list))

  (for-each
   (lambda (user)
     (define first (cadr user))
     (define pass (cadr first))
     (hashmap-set! passwords pass #t))
   auth-lines)

  (context-ctr passwords database tokens))

(define callcontext/p
  (make-parameter #f))

(define (query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define decoded (uri-decode sp))
           (define-values (key eq val) (string-split-3 #\= decoded))
           (when (string-null? eq) (raisu 'bad-query query))
           (cons (string->symbol key) val))
         split))
  (alist->hashmap key-values))

(define (make-callcontext break request body)
  (define uri (request-uri request))
  (define query/encoded (uri-query uri))
  (define qH (if query/encoded (query->hashmap query/encoded) (make-hashmap)))
  (callcontext-ctr break request qH body #f))

(define (make-handler)
  (lambda (request body)
    (call-with-current-continuation
     (lambda (k)
       (parameterize ((callcontext/p (make-callcontext k request body)))
         (handler request body))))))

(define (tegfs-serve/parse)
  (dprintln "Starting the server")
  (parameterize ((context/p (make-context)))
    (run-server (make-handler) 'http '(#:port 8081))))
