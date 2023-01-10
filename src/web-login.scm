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

%use (assq-or) "./euphrates/assq-or.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-headers callcontext-url) "./web-callcontext.scm"
%use (web::get-domainname) "./web-get-domainname.scm"
%use (web::redirect) "./web-redirect.scm"

(define (web::login)
  (define callctx (web::callcontext/p))
  (define headers (callcontext-headers callctx))
  (define domainname
    (web::get-domainname callctx))
  (define referer0
    (assq-or 'referer headers #f))
  (define referer1
    (or referer0 "home"))
  (define referer2
    (remove-common-prefix referer1 domainname))
  (define referer
    (if (equal? referer2 (callcontext-url callctx))
        "home"
        referer2))
  (define target
    (stringf "auth?yes=~a" (uri-encode referer)))

  (web::redirect callctx target #f))
