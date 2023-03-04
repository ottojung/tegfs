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

(cond-expand
 (guile
  (define-module (tegfs web-make-callcontext)
    :export (web::make-callcontext web::make-callcontext/redirect)
    :use-module ((euphrates hashmap) :select (hashmap-ref hashmap? make-hashmap))
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((tegfs web-callcontext) :select (add-callcontext-respheaders! callcontext-ctr callcontext-headers callcontext-respheaders callcontext-token))
    :use-module ((tegfs web-get-cookie) :select (web::get-cookie))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-query-to-hashmap) :select (web::query->hashmap))
    :use-module ((tegfs web-set-cookie-header) :select (web::set-cookie-header))
    :use-module ((tegfs web-token-override-p) :select (web::token-override/p))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(cond-expand
 (guile

  (use-modules (web request))
  (use-modules (web uri))

  ))

(define (remember-to-set-access-token callctx ret)
  (define result
    (webcore::ask
     `(whats (time-left ,ret TL))))
  (define expiery
    (web::iterate-profun-results
     :or (lambda _ 0)
     :results result
     (TL) TL))
  (define additional-headers
    (list (web::set-cookie-header "key" ret expiery)))
  (add-callcontext-respheaders! callctx additional-headers))

(define (get-access-token callctx queryfn headers)
  (or (web::token-override/p)
      (let ((qH (queryfn)))
        (or
         (let ((ret (hashmap-ref qH 'key #f)))
           (when ret (remember-to-set-access-token callctx ret))
           ret)
         (web::get-cookie "key" headers)
         (web::get-cookie "pwdtoken" headers)))))

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
          ((referer)
           (cond
            ((string? (cdr p)) p)
            (else (cons 'referer (uri->string (cdr p))))))
          (else p)))
      headers)))

  (letrec
      ((tokenfn (memconst (get-access-token callctx queryfn headers)))
       (callctx (callcontext-ctr url path headersfn queryfn body '() tokenfn)))
    callctx))

(define (web::make-callcontext/redirect callctx new-url new-path new-query/encoded new-body)
  (define queryfn
    (if (hashmap? new-query/encoded)
        (lambda _ new-query/encoded)
        (memconst (initialize-query new-query/encoded))))

  (define headers
    (append
     (callcontext-respheaders callctx)
     (callcontext-headers callctx)))
  (define headersfn (const headers))

  (letrec
      ((tokenfn (memconst (or (get-access-token ret queryfn headers)
                              (callcontext-token callctx))))
       (ret (callcontext-ctr new-url new-path headersfn queryfn new-body '() tokenfn)))
    ret))
