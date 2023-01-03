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

%var web-make-callcontext

%use (alist->hashmap hashmap-delete! hashmap-ref make-hashmap) "./euphrates/hashmap.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-split-3) "./euphrates/string-split-3.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (permission-still-valid?) "./permission-still-valid-huh.scm"
%use (permission-token) "./permission.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-ctr callcontext-request set-callcontext-key!) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-tokens) "./web-context.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-try-uri-decode) "./web-try-uri-decode.scm"

%for (COMPILER "guile")

(use-modules (web request))
(use-modules (web uri))

%end

(define (set-user-key! key)
  (set-callcontext-key! (web-callcontext/p) key))

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

(define (get-access-token callctx)
  (or
   (let* ((qH (callcontext-query callctx))
          (ret (hashmap-ref qH 'key #f)))
     (when ret (set-user-key! ret))
     ret)
   (let ((request (callcontext-request callctx)))
     (or (get-cookie "key" request)
         (get-cookie "pwdtoken" request)))))

(define (query->hashmap query)
  (define split (string-split/simple query #\&))
  (define key-values
    (map (lambda (sp)
           (define-values (key eq val) (string-split-3 #\= sp))
           (cons (string->symbol key) (web-try-uri-decode val)))
         split))
  (alist->hashmap key-values))

(define (initialize-query request)
  (define uri (request-uri request))
  (define query/encoded (uri-query uri))
  (if query/encoded
      (query->hashmap query/encoded)
      (make-hashmap)))

(define (web-make-callcontext break request body)
  (define qH (memconst (initialize-query request)))
  (letrec
      ((tokenfn (memconst (get-access-token callctx)))
       (callctx (callcontext-ctr break request qH body #f tokenfn)))
    callctx))
