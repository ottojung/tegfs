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

%use (root/p) "./root-p.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (tegfs-process-categorization-text) "./edit-tags.scm"
%use (tegfs-add) "./add.scm"
%use (sha256sum) "./sha256sum.scm"

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

(define upload-registry-filename "upload/upload.tegfs.yaml")

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

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

  (cont (build-response
         #:code status
                     ;; most of these settings come from here: https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html
         #:headers `((referrer-policy . "strict-origin-when-cross-origin") ;; optional, ensures not to send too much user data.
                     (x-frame-options . "DENY") ;; optional, bans embedding in <iframe> and such.
                     (strict-transport-security . "max-age=63072000; includeSubDomains; preload") ;; something something security.
                     ;; TODO: add more SECURITY!!!!
                     (content-type . (,content-type ,@content-type-params))
                     ,@extra-headers))
        (lambda (port)
          (parameterize ((current-output-port port))
            (when doctype (display doctype))
            (display "<html>\n")
            (display "<head>\n")
            (when title
              (display "  <title>")
              (display title)
              (display "</title>\n"))
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
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (set-cookie-header key value)
  ;; TODO: make cookies expire!!!!!
  (cons 'set-cookie
        (string-append (~a key) "=" (~a value)
                       " ; HttpOnly ; Secure ; SameSite=Lax ;")))

(define login-style
  "<style>

.subc {
  position: absolute;
  top: 30%;
  left: 50%;
  transform: translateY(-50%);
  transform: translateX(-50%);
}

form {
  border: 3px solid #f1f1f1;
}

input[type=text], input[type=password] {
  width: 100%;
  padding: 12px 20px;
  margin: 8px 0;
  display: inline-block;
  border: 1px solid #ccc;
  box-sizing: border-box;
}

/* Set a style for all buttons */
button {
  background-color: #04AA6D;
  color: white;
  padding: 14px 20px;
  margin: 8px 0;
  border: none;
  cursor: pointer;
  width: 100%;
}

/* Add a hover effect for buttons */
button:hover {
  opacity: 0.8;
}

/* Center the avatar image inside this container */
.imgcontainer {
  text-align: center;
  margin: 24px 0 12px 0;
}

/* Add padding to containers */
.container {
  padding: 16px;
}

span.psw {
  float: right;
  padding-top: 16px;
}

</style>")

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
  (lambda _
    (respond xml #:extra-heads (list login-style))))

(define (static-error-message status message)
  (define xml (message-template message))
  (lambda _
    (respond xml #:status status #:extra-heads (list login-style))))

(define login-failed-body
  (form-template #f "
       <label><b>You are a failure</b></label>
       <input type='password' placeholder='Enter Password' name='psw' required autofocus>
       <button type='submit'>Login</button>"))

(define login-success-body
  (form-template #f "<label><b>Loged in just fine</b></label>"))

(define (login)
  (define head-style login-style)

  (define body login-body)

  (respond body #:extra-heads `(,head-style)))

(define (list-singleton? lst)
  (and (not (null? lst))
       (null? (cdr lst))))

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
               #:extra-heads `(,login-style)
               #:extra-headers (list (set-cookie-header "access" access-token))
               )
      (respond login-failed-body #:extra-heads `(,login-style))))

(define permission-denied
  (static-error-message 401 "Permission denied"))

(define (check-permissions)
  (define callctx (callcontext/p))
  (define request (callcontext-request callctx))
  (define headers (request-headers request))
  (define cookies (assoc 'cookie headers))

  (unless (pair? cookies)
    (permission-denied))

  (unless (string? (cdr cookies))
    (raisu 'bad-cookies-cdr cookies))

  (define cookie-split
    (string-split/simple (cdr cookies) #\=))

  (unless (= 2 (length cookie-split))
    (raisu 'bad-cookie-split cookie-split cookies))

  (define cookie-type (car cookie-split))
  (define cookie-value (cadr cookie-split))

  (unless (equal? "access" cookie-type)
    (raisu 'bad-cookie-type cookie-type cookies))

  (define ctx (context/p))
  (define tokens (context-tokens ctx))
  (define existing (hashmap-ref tokens cookie-value #f))

  (values))

(define (get-random-basename)
  (list->string
   (random-choice 30 alphanum-lowercase/alphabet)))

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
  (define body/parsed (parse-multipart/from-data body/bytes))
  (define body/hash (parsed-multipart->hashmap-by-name body/parsed))

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
  (respond (make-upload-body) #:extra-heads (list login-style)))

(define (cookie1)
  (define request (callcontext-request (callcontext/p)))
  (respond
   `(p (b "Hi!") (br) "there. Your headers are the following:"
       ,(~s (request-headers request)))
   #:extra-headers `(,(set-cookie-header "hi" "there"))))

(define (hacker)
  (values '((content-type . (text/plain))) "Hello hacker!"))

(define (handler request body)
  (define path-components
    (request-path-components request))

  (dprintln "Got request: ~s" path-components)

  (if (null? (length path-components))
      (not-found)
      (case (string->symbol (car path-components))
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

(define (parse-content-disposition key/str value)
  (define key (string->symbol key/str))

  (define parts
    (map string-strip (string-split/simple value #\;)))

  (define head
    (car parts))

  (define args
    (cdr parts))

  (define key-values-0
    (map (lambda (part)
           (define-values (pre it post) (string-split-3 #\= part))
           (when (string-null? it)
             (raisu 'key-value-is-wrong part value))
           (cons pre post))
         args))

  (define key-values
    (cons (cons "head" head) key-values-0))

  (cons
   (cons key value)
   (map (fn-cons (comp (string-append key/str ":") string->symbol) (comp un~s ~a))
        key-values)))

(define (parse-multipart-type type-chunk)
  (define lines
    (filter (negate string-null?)
            (map string-strip (string->lines type-chunk))))

  (define split1
    (apply
     append
     (map
      (lambda (line)
        (define-values (pre0 it post0) (string-split-3 #\: line))
        (when (string-null? it)
          (throw 'type-chunk-line-must-contain-a-column line))

        (define pre1 (string-strip pre0))
        (define pre (string->symbol pre1))
        (define post (uri-decode (string-strip post0)))

        (case pre
          ((Content-Disposition) (parse-content-disposition pre1 post))
          (else (list (cons pre post)))))
      lines)))

  split1)

(define (parsed-multipart->hashmap-by-name parsed)
  (define H (make-hashmap))
  (for-each
   (lambda (block)
     (define name (assoc 'Content-Disposition:name block))
     (unless name
       (raisu 'parsed-block-does-not-have-a-name block))
     (hashmap-set! H (cdr name) block))
   parsed)
  H)

;; TODO: contribute this procedure to guile/module/web ?
(define (parse-multipart/from-data data)
  (define len (bytevector-length data))
  (define header-size
    (let loop ((i 0))
      (if (>= i len) (throw 'multipart-data-header-not-found len)
          (let ((cur (array-ref data i)))
            (if (equal? 13 cur) i
                (loop (+ 1 i)))))))

  (define (bytevector-crop start end/non-inclusive source)
    (let* ((size (- end/non-inclusive start))
           (vec (make-bytevector size 0)))
      (bytevector-copy! source start vec 0 size)
      vec))

  (define (bytevector-prefix? prefix-vector original-vector start)
    (define len (bytevector-length prefix-vector))
    (define olen (- (bytevector-length original-vector) start))
    (and (<= len olen)
         (let loop ((i 0))
           (or (>= i len)
               (and (equal? (array-ref prefix-vector i)
                            (array-ref original-vector (+ start i)))
                    (loop (+ 1 i)))))))

  (define (bytevector-append a b)
    (define len-a (bytevector-length a))
    (define len-b (bytevector-length b))
    (define len-ret (+ len-a len-b))
    (define ret (make-bytevector len-ret 0))
    (bytevector-copy! a 0 ret 0 len-a)
    (bytevector-copy! b 0 ret len-a len-b)
    ret)

  (define data-newline #vu8(13 10))
  (define block-data-separator #vu8(13 10 13 10))
  (define block-data-separator-length (bytevector-length block-data-separator))
  (define block-data-start #vu8(45 45))

  (define header
    (bytevector-append
     data-newline (bytevector-crop 0 header-size data)))

  (define header-length
    (bytevector-length header))

  (define header/string
    (bytevector->string header "ascii"))

  (define (split-block start end/non-inclusive)

    (define separator-index
      (let loop ((i start))
        (if (>= i end/non-inclusive) #f
            (if (bytevector-prefix? block-data-separator data i)
                i
                (loop (+ 1 i))))))

    (define crop-index
      (or separator-index end/non-inclusive))

    (define type-chunk
      (bytevector->string
       (bytevector-crop start crop-index data) "utf-8"))

    ;; TODO: should require data?
    (define data-chunk
      (if separator-index
          (bytevector-crop
           (+ separator-index block-data-separator-length)
           end/non-inclusive
           data)
          #vu8()))

    (cons type-chunk data-chunk))

  (define blocks
    (let loop ((i header-size)
               (block-start header-size)
               (buf '()))
      (if (>= i len)
          ;; TODO: should throw an exception? last block must end explicitly by mentioning separator that ends with --
          (reverse
           (cons (split-block block-start (min len i)) buf))
          (if (bytevector-prefix? header data i)
              (let ((new-start (+ i header-length)))
                (if (bytevector-prefix? block-data-start data new-start)
                    (reverse
                     (cons (split-block block-start i) buf))
                    (loop new-start new-start
                          (cons (split-block block-start i) buf))))
              (loop (+ 1 i) block-start buf)))))

  (define parsed-blocks
    (map (lambda (p)
           (cons
            (cons 'data (cdr p))
            (parse-multipart-type (car p))))
         blocks))

  parsed-blocks)

(define (tegfs-serve/parse)
  (dprintln "Starting the server")
  (parameterize ((context/p (make-context)))
    (run-server (make-handler) 'http '(#:port 8081))))
