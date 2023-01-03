;;;; Copyright (C) 2022, 2023  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

%run guile

%var tegfs-serve/parse

%use (comp) "./euphrates/comp.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (directory-files) "./euphrates/directory-files.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (fn) "./euphrates/fn.scm"
%use (alist->hashmap hashmap-delete! hashmap-foreach hashmap-ref make-hashmap) "./euphrates/hashmap.scm"
%use (hashset-has? list->hashset) "./euphrates/hashset.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (path-without-extension) "./euphrates/path-without-extension.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-split-3) "./euphrates/string-split-3.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (current-time/p) "./current-time-p.scm"
%use (default-login-expiery-time) "./default-login-expiery-time.scm"
%use (filemap-delete-by-recepientid! filemap-ref-by-recepientid) "./filemap.scm"
%use (make-permission!) "./make-permission-bang.scm"
%use (permission-still-valid?) "./permission-still-valid-huh.scm"
%use (permission-admin? permission-filemap permission-token) "./permission.scm"
%use (sha256sum) "./sha256sum.scm"
%use (sharedinfo-ctime sharedinfo-stime) "./sharedinfo.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-body-not-found) "./web-body-not-found.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-ctr callcontext-request set-callcontext-key!) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-filemap/2 context-passwords context-port context-sharedir context-tokens) "./web-context.scm"
%use (web-details) "./web-details.scm"
%use (web-directory) "./web-directory.scm"
%use (web-full) "./web-full.scm"
%use (web-get-permissions) "./web-get-permissions.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-login-body) "./web-login-body.scm"
%use (web-login-failed-body) "./web-login-failed-body.scm"
%use (web-login-success-body) "./web-login-success-body.scm"
%use (web-make-context) "./web-make-context.scm"
%use (web-message-template) "./web-message-template.scm"
%use (web-not-found) "./web-not-found.scm"
%use (web-preview-height) "./web-preview-height.scm"
%use (web-preview-width) "./web-preview-width.scm"
%use (web-query) "./web-query.scm"
%use (web-respond) "./web-respond.scm"
%use (web-return!) "./web-return-bang.scm"
%use (web-set-cookie-header) "./web-set-cookie-header.scm"
%use (web-share) "./web-share.scm"
%use (web-static-error-message) "./web-static-error-message.scm"
%use (web-style) "./web-style.scm"
%use (web-try-uri-decode) "./web-try-uri-decode.scm"
%use (web-upload) "./web-upload.scm"
%use (web-uploadcont) "./web-uploadcont.scm"
%use (web-url-icon/svg) "./web-url-icon-svg.scm"
%use (with-current-time) "./with-current-time.scm"

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

(define (main.css)
  (web-return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((content-type . (text/css))
              (Cache-Control . "max-age=3600, public, private"))))
   web-style))

(define (static-message message)
  (define xml (web-message-template message))
  (lambda _ (web-respond xml)))

(define (login)
  (web-respond web-login-body))

(define (set-user-key! key)
  (set-callcontext-key! (web-callcontext/p) key))

(define (logincont)
  (define body/bytes (callcontext-body (web-callcontext/p)))

  (define _4
    (unless body/bytes
      (web-body-not-found)))

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

  (define ctx (web-context/p))
  (define passwords (context-passwords ctx))
  (define registered? (hashmap-ref passwords passw #f))
  (define admin? #t) ;; TODO: read from the config
  (define uploadaccess? #t) ;; TODO: read from the config
  (define detailsaccess? #t) ;; TODO: read from the config
  (define share-longer-than-view? #t) ;; TODO: read from the config

  (if registered?
      (let* ((perm (make-permission! ctx default-login-expiery-time admin? uploadaccess? detailsaccess? share-longer-than-view?))
             (token (permission-token perm)))
        (web-respond
         web-login-success-body
         #:extra-headers (list (web-set-cookie-header "pwdtoken" token))))
      (web-respond web-login-failed-body)))

(define permission-denied
  (web-static-error-message 401 "Permission denied"))

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
  (define perm (web-get-permissions))
  (unless (and perm (permission-admin? perm))
    (permission-denied)))

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
  (web-return! unavailable-response unavailable-bytevector))

(define (preview-unknownurl)
  (web-return! unknownurl-response unknownurl-bytevector))

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
  (web-return! unknownurl-response unknownurl-bytevector))

(define (invalidate-permission perm)
  (define ctx (web-context/p))
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
  (define now (or (current-time/p)
                  (raisu 'current-time-is-not-set)))

  (collectgarbage/nocall now)

  (web-return!
   (build-response
    #:code 200
    #:headers
    (append web-basic-headers
            `((Cache-Control . "no-cache"))))
   "ok\n"))

(define (collectgarbage/nocall now)
  (define ctx (web-context/p))
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
   (lambda (recepientid info)
     (unless (sharedinfo-still-valid? info)
       (delayop
        (display "UNSHARE ") (write recepientid) (newline)
        (filemap-delete-by-recepientid! filemap/2 recepientid))))
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
     (define recepientid (path-without-extension sharedname))
     (define info (filemap-ref-by-recepientid filemap/2 recepientid #f))
     (unless info
       (display "File not shared: ")
       (write sharedname)
       (display " deleting...\n")
       (file-delete full-name)))
   (directory-files sharedir)))

(define handlers-config
  `((/login ,login public)
    (/logincont ,logincont public)
    (/main.css ,main.css public)
    (/collectgarbage ,collectgarbage public)
    (/query ,web-query public)
    (/directory ,web-directory public)
    (/details ,web-details public)
    (/full ,web-full public)
    (/upload ,web-upload public)
    (/uploadcont ,web-uploadcont public)
    (/previewunknown ,previewunknown)
    (/previewunknownurl ,previewunknownurl)
    (/share ,web-share public)
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

(define (log-request request)
  (define uri (request-uri request))
  (display "Got request: ") (display (uri-path uri))
  (let ((q (uri-query uri)))
    (when q (display "?") (display q)))
  (display "\n")
  )

(define (handler request body)
  (define path (uri-path (request-uri request)))

  (let* ((target path)
         (func (hashmap-ref handlers-funcmap target #f))
         (public? (hashset-has? handlers-publicset target)))
    (unless func (web-not-found))
    (unless public? (check-permissions))
    (func)))

(define (query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define-values (key eq val) (string-split-3 #\= sp))
           (cons (string->symbol key) (web-try-uri-decode val)))
         split))
  (alist->hashmap key-values))

(define (get-access-token)
  (or
   (let* ((qH (web-get-query))
          (ret (hashmap-ref qH 'key #f)))
     (when ret (set-user-key! ret))
     ret)
   (let* ((callctx (web-callcontext/p))
          (request (callcontext-request callctx)))
     (or (get-cookie "key" request)
         (get-cookie "pwdtoken" request)))))

(define (initialize-permissions)
  (define token (get-access-token))
  (define ctx (web-context/p))
  (define tokens (context-tokens ctx))
  (define existing (hashmap-ref tokens token #f))

  (and existing
       (if (permission-still-valid? existing) existing
           (begin
             (invalidate-permission existing)
             #f))))

(define (initialize-query request)
  (define uri (request-uri request))
  (define query/encoded (uri-query uri))
  (if query/encoded
      (query->hashmap query/encoded)
      (make-hashmap)))

(define (make-callcontext break request body)
  (define qH (memconst (initialize-query request)))
  (define perm (memconst (initialize-permissions)))
  (callcontext-ctr break request qH body #f perm))

(define (make-handler)
  (lambda (request body)
    (log-request request)
    (call-with-current-continuation
     (lambda (k)
       (parameterize ((web-callcontext/p (make-callcontext k request body)))
         (with-current-time
          (handler request body)))))))

(define (tegfs-serve/parse)
  (dprintln "Starting the server")
  (parameterize ((web-context/p (web-make-context)))
    (let ((port (context-port (web-context/p))))

      (dprintln "Collecting garbage left from the previous run...")
      (collectgarbage/nocall (time-get-current-unixtime))
      (dprintln "Done")

      (run-server (make-handler) 'http `(#:port ,port)))))
