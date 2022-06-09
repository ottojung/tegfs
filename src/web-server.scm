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
%use (make-hashmap hashmap-ref hashmap-set! hashmap->alist) "./euphrates/ihashmap.scm"
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
%use (sha256sum) "./sha256sum.scm"
%use (parse-multipart-as-hashmap) "./web-parse-multipart.scm"
%use (web-make-response) "./web-make-response.scm"
%use (get-random-basename) "./get-random-basename.scm"
%use (web-set-cookie-header) "./web-set-cookie-header.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-style) "./web-style.scm"

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
  (callcontext-ctr break request body) callcontext?
  (break callcontext-break) ;; break handler
  (request callcontext-request) ;; client request
  (body callcontext-body) ;; client body
  )

(define upload-registry-filename "upload/upload.tegfs.reg.lisp")

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (return! stats body)
  (define callctx (callcontext/p))
  (define cont (callcontext-break callctx))
  (cont stats body))

(define* (respond . args)
  (define-values (stats body) (apply web-make-response args))
  (return! stats body))

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
    #:headers (append web-basic-headers `((content-type . (text/css)))))
   web-style))

;; <label for='uname'><b>Username</b></label>
;; <input type='text' placeholder='Enter Username' name='uname' required autofocus>

(define (form-template form-params insides)
  (string-append
   "<div class='root1'><div class='subc'><form "
   (or form-params "")
   " method='post'>
     <div class='container'>"
   insides
   "  </div>
    </form></div></div>"))

(define login-body
  (form-template "action='logincont' enctype='application/x-www-form-urlencoded'" "
    <input type='password' placeholder='Enter Password' name='psw' required autofocus>
    <button type='submit'>Login</button>"))

(define (message-template message)
  (define xml
    (with-output-to-string
      (lambda _
        (sxml->xml `(label (b ,message))))))
  (form-template #f xml))

(define (static-message message)
  (define xml (message-template message))
  (lambda _ (respond xml)))

(define (static-error-message status message)
  (define xml (message-template message))
  (lambda _
    (respond xml #:status status)))

(define login-failed-body
  (form-template #f "
       <label><b>You are a failure</b></label>
       <input type='password' placeholder='Enter Password' name='psw' required autofocus>
       <button type='submit'>Login</button>"))

(define login-success-body
  (form-template #f "<label><b>Loged in just fine</b></label>"))

(define (login)
  (respond login-body))

(define (generate-access-token)
  (list->string (random-choice 60 alphanum-lowercase/alphabet)))

(define body-not-found
  (static-error-message 417 "Send user body"))

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
  (define access-token (and registered? (generate-access-token)))

  (when registered?
    (hashmap-set! tokens access-token #t))

  (if registered?
      (respond login-success-body
               #:extra-headers (list (web-set-cookie-header "access" access-token))
               )
      (respond login-failed-body)))

(define permission-denied
  (static-error-message 401 "Permission denied"))

(define (parse-cookies-string cookies/string)
  (unless (string? cookies/string)
    (raisu 'bad-cookies-cdr cookies/string))

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
  (define headers (request-headers request))
  (define cookies-p (assoc 'cookie headers))

  (unless (pair? cookies-p)
    (permission-denied))

  (define cookies/string (cdr cookies-p))
  (define cookies (parse-cookies-string cookies/string))

  (define access-cookie
    (let ((got (assoc "access" cookies)))
      (and got (cdr got))))

  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define existing (hashmap-ref tokens access-cookie #f))

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
  (define _42
    (check-permissions))

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

  (when full-filename
    (make-directories (dirname full-filename))
    (let ((port (open-file-port full-filename "w")))
      (put-bytevector port file-content)
      (close-port port)))

  (define tags-list-result
    (tegfs-process-categorization-text tags))

  ;; TODO: edit the categorization file
  (define tags-list
    (case (car tags-list-result)
      ((ok) (cdr tags-list-result))
      ((error) ((error-tags-list (cdr tags-list-result))))
      ((duplicates) (duplicates-tags-list))
      (else (raisu 'unknown-tags-list-result))))

  (tegfs-add
   <target> title tags-list
   #f ;; TODO: accept series? flag
   '() ;; TODO: accept key-value-pairs from the user
   upload-registry-filename #f
   )

  ((upload-success-page <target>)))

(define (make-upload-body)
  (define categorization-file (append-posix-path (root/p) categorization-filename))
  (define tags-value (read-string-file categorization-file))

  (define inner
    (with-output-to-string
      (lambda _
        (printf "
    <input type='text' placeholder='Enter title' name='title' >
    <input type='file' name='file' >
    <textarea rows='10' cols='120' name='tags'>~a</textarea>
    <button type='submit'>Upload</button>"
                tags-value))))

  (form-template "action='uploadcont' enctype='multipart/form-data'" inner))

(define (upload)
  (check-permissions)
  (respond (make-upload-body)))

(define (cookie1)
  (define request (callcontext-request (callcontext/p)))
  (respond
   `(p (b "Hi!") (br) "there. Your headers are the following:"
       ,(~s (request-headers request)))
   #:extra-headers `(,(web-set-cookie-header "hi" "there"))))

(define (hacker)
  (values '((content-type . (text/plain))) "Hello hacker!"))

(define (handler request body)
  (define path-components
    (request-path-components request))

  (dprintln "Got request: ~s" path-components)

  (if (null? path-components)
      (not-found)
      (case (string->symbol (car path-components))
        ((main.css) (main.css))
        ((login) (login))
        ((logincont) (logincont))
        ((upload) (upload))
        ((uploadcont) (uploadcont))
        ((cookie1) (cookie1))
        ((hacker) (hacker))
        (else (not-found)))))

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

(define (make-callcontext break request body)
  (callcontext-ctr break request body))

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
