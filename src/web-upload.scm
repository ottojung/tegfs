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

%use (fn-alist) "./euphrates/fn-alist.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-token) "./web-callcontext.scm"
%use (web::handle-profun-results) "./web-handle-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::make-upload-body) "./web-make-upload-body.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::upload)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))

  (define result
    (webcore::ask
     `(whats (key ,key) (categorization C))))

  (web::handle-profun-results
   result
   (lambda (equals)
     (fn-alist
      (C)
      (web::make-html-response
       (web::make-upload-body C))))))
