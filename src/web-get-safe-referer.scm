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

%var web::get-safe-referer

%use (assq-or) "./euphrates/assq-or.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (url-get-hostname-and-port) "./euphrates/url-get-hostname-and-port.scm"
%use (callcontext-headers callcontext-url) "./web-callcontext.scm"
%use (web::get-domainname) "./web-get-domainname.scm"

(define (web::get-safe-referer callctx)
  (define headers
    (callcontext-headers callctx))
  (define domainname
    (web::get-domainname callctx))
  (define referer0
    (assq-or 'referer headers #f))
  (define referer1
    (or referer0 "home"))
  (define referer2
    (if (equal? (url-get-hostname-and-port domainname)
                (url-get-hostname-and-port referer1))
        (remove-common-prefix referer1 domainname)
        referer1))
  (define referer
    (if (equal? referer2 (callcontext-url callctx))
        "home"
        referer2))
  referer)
