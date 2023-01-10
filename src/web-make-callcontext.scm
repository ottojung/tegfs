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

%var web::make-callcontext
%var web::make-callcontext/raw

%use (hashmap-ref hashmap? make-hashmap) "./euphrates/hashmap.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (add-callcontext-respheaders! callcontext-ctr) "./web-callcontext.scm"
%use (web::get-cookie) "./web-get-cookie.scm"
%use (web::query->hashmap) "./web-query-to-hashmap.scm"
%use (web::set-cookie-header) "./web-set-cookie-header.scm"

%for (COMPILER "guile")

(use-modules (web request))
(use-modules (web uri))

%end

(define (get-access-token callctx qH headers)
  (or
   (let ((ret (hashmap-ref qH 'key #f)))
     (when ret
       (let ()
         (define additional-headers
           (list (web::set-cookie-header "key" ret)))
         (add-callcontext-respheaders! callctx additional-headers)))
     ret)
   (or (web::get-cookie "key" headers)
       (web::get-cookie "pwdtoken" headers))))

(define (initialize-query query/encoded)
  (if query/encoded
      (web::query->hashmap query/encoded)
      (make-hashmap)))

(define (web::make-callcontext req body)
  (define uri (request-uri req))
  (define path (uri-path uri))
  (define url (uri->string uri))
  (define query/encoded (uri-query uri))
  (define headers (request-headers req))
  (web::make-callcontext/raw url path query/encoded headers body))

(define (web::make-callcontext/raw url path query headers body)
  (define queryfn
    (if (hashmap? query)
        (lambda _ query)
        (memconst (initialize-query query))))
  (define headersfn
    (memconst
     (map
      (lambda (p)
        (define key (car p))
        (case key
          ((referer) (cons 'referer (uri->string (cdr p))))
          (else p)))
      headers)))

  (letrec
      ((tokenfn (memconst (get-access-token callctx (queryfn) headers)))
       (callctx (callcontext-ctr url path headersfn queryfn body '() tokenfn)))
    callctx))
