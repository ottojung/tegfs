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

%var web::login

%use (stringf) "./euphrates/stringf.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (web::get-safe-referer) "./web-get-safe-referer.scm"
%use (web::redirect) "./web-redirect.scm"

(define (web::login)
  (define callctx (web::callcontext/p))
  (define referer (web::get-safe-referer callctx))
  (define target
    (stringf "auth?yes=~a" (uri-encode referer)))

  (web::redirect callctx target #f))
