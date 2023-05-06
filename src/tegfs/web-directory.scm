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
  (define-module (tegfs web-directory)
    :export (web::directory)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((tegfs default-full-sharing-time) :select (default-full-sharing-time))
    :use-module ((tegfs default-preview-sharing-time) :select (default-preview-sharing-time))
    :use-module ((tegfs web-bad-request) :select (web::bad-request))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-token))
    :use-module ((tegfs web-get-query) :select (web::get-query))
    :use-module ((tegfs web-handle-profun-results) :select (web::handle-profun-results))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-query-display-results) :select (web::query-display-results))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::directory)
  (define callctx (web::callcontext/p))
  (define ctxq (web::get-query))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define select? (equal? "on" (hashmap-ref ctxq 'select #f)))
  (define show-filter? #f)

  (if (not vid)
      (web::bad-request "Request query missing requiered 'd' argument")
      (web::handle-profun-results
       (webcore::ask
        `(whats
          (key ,(callcontext-token callctx))
          (shared-entry-contains ,vid E)
          (share-preview E ,default-preview-sharing-time _ATP _P)
          (share-full E ,default-full-sharing-time _ATF F)
          (link-shared _P PL)
          more (99999)
          ))
       (web::directory-handle callctx select? show-filter?))))

(define (web::directory-handle callctx select? show-filter?)
  (define maybe-value #f)
  (define show-menu? (not select?))
  (define initial? #f)

  (lambda (equals)
    (web::make-html-response
     (lambda _
       (display "<br/>")
       (web::query-display-results callctx initial? select? show-filter? maybe-value show-menu? equals)))))
