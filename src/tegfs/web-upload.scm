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

%var web::upload

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::iterate-profun-results) "./web-iterate-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::make-upload-body) "./web-make-upload-body.scm"
%use (web::uploadcont) "./web-uploadcont.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::upload::initial callctx)
  (define key (callcontext-token callctx))

  (define result
    (webcore::ask
     `(whats (key ,key) (categorization C))))

  (web::iterate-profun-results
   result (C)
   (web::make-html-response
    (web::make-upload-body C))))

(define (web::upload)
  (define callctx (web::callcontext/p))
  (define query (callcontext-query callctx))
  (define continue-v (hashmap-ref query 'continue #f))
  (define continue? (equal? "on" continue-v))

  (if continue?
      (web::uploadcont)
      (web::upload::initial callctx)))
