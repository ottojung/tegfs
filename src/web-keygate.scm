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

%var web::keygate

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (web::bad-request) "./web-bad-request.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::permission-denied) "./web-permission-denied.scm"
%use (web::return) "./web-return.scm"

(define (web::keygate)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define query (callcontext-query callctx))
  (define expected-key (hashmap-ref query 'expected #f))
  (define continue (hashmap-ref query 'continue #f))

  (if (not expected-key)
      (web::bad-request "Missing expected query argument ~s" "expected")
      (if (not continue)
          (web::bad-request "Missing expected query argument ~s" "continue")
          (if (equal? key expected-key)
              (web::return
               301
               `((Location . ,continue)
                 (Cache-Control . "no-cache"))
               #f)
              (web::permission-denied)))))
