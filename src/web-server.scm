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

%use (dprintln) "./euphrates/dprintln.scm"
%use (alist->hashmap hashmap-ref) "./euphrates/hashmap.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (web-collectgarbage web-collectgarbage/nocall) "./web-collectgarbage.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-port) "./web-context.scm"
%use (web-details) "./web-details.scm"
%use (web-directory) "./web-directory.scm"
%use (web-full) "./web-full.scm"
%use (web-login) "./web-login.scm"
%use (web-logincont) "./web-logincont.scm"
%use (web-make-callcontext) "./web-make-callcontext.scm"
%use (web-make-context) "./web-make-context.scm"
%use (web-make-html-response) "./web-make-html-response.scm"
%use (web-message-template) "./web-message-template.scm"
%use (web-not-found) "./web-not-found.scm"
%use (web-preview-height) "./web-preview-height.scm"
%use (web-preview-width) "./web-preview-width.scm"
%use (web-query) "./web-query.scm"
%use (web-return!) "./web-return-bang.scm"
%use (web-share) "./web-share.scm"
%use (web-static-error-message) "./web-static-error-message.scm"
%use (web-style) "./web-style.scm"
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
  (lambda _ (web-make-html-response xml)))

(define permission-denied
  (web-static-error-message 401 "Permission denied"))

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

(define handlers-config
  `((/login ,web-login)
    (/logincont ,web-logincont)
    (/main.css ,main.css)
    (/collectgarbage ,web-collectgarbage)
    (/query ,web-query)
    (/directory ,web-directory)
    (/details ,web-details)
    (/full ,web-full)
    (/upload ,web-upload)
    (/uploadcont ,web-uploadcont)
    (/previewunknown ,previewunknown)
    (/previewunknownurl ,previewunknownurl)
    (/share ,web-share)
    ))

(define handlers-funcmap
  (alist->hashmap
   (map
    (lambda (p) (cons (~a (car p)) (cadr p)))
    handlers-config)))

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
         (func (hashmap-ref handlers-funcmap target #f)))
    (unless func (web-not-found))
    (func)))

(define (make-handler)
  (lambda (request body)
    (log-request request)
    (call-with-current-continuation
     (lambda (k)
       (parameterize ((web-callcontext/p (web-make-callcontext k request body)))
         (with-current-time
          (handler request body)))))))

(define (tegfs-serve/parse)
  (dprintln "Starting the server")
  (parameterize ((web-context/p (web-make-context)))
    (let ((port (context-port (web-context/p))))

      (dprintln "Collecting garbage left from the previous run...")
      (web-collectgarbage/nocall (time-get-current-unixtime))
      (dprintln "Done")

      (run-server (make-handler) 'http `(#:port ,port)))))
