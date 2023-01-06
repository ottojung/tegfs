;;;; Copyright (C) 2023  Otto Jung
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

%var web::redirect

%use (url-get-path) "./euphrates/url-get-path.scm"
%use (url-get-query) "./euphrates/url-get-query.scm"
%use (callcontext-headers) "./web-callcontext.scm"
%use (web::make-callcontext/raw) "./web-make-callcontext.scm"
%use (web::server-current-handler/p) "./web-server-current-handler-p.scm"

(define (web::redirect callctx new-url new-body)
  (define handler (web::server-current-handler/p))
  (define old-headers (callcontext-headers callctx))
  (define new-path (url-get-path new-url))
  (define new-query/encoded (url-get-query new-url))
  (define new-callctx
    (web::make-callcontext/raw
     new-url new-path new-query/encoded old-headers
     new-body))

  (handler new-callctx))
