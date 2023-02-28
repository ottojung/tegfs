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
  (define-module (tegfs web-get-domainname)
    :export (web::get-domainname)
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs web-callcontext) :select (callcontext-headers))
    )))



(define (web::get-domainname callctx)
  (define headers (callcontext-headers callctx))
  (define host/get (assoc 'host headers))
  (define protocol "http") ;; TODO: get it from somewhere?

  (and host/get
       protocol
       (let* ((host/pair (cdr host/get))
              (host
               (if (pair? host/pair)
                   (if (cdr host/pair)
                       (string-append (~a (car host/pair)) ":" (~a (cdr host/pair)))
                       (~a (car host/pair)))
                   host/pair)))
         (string-append protocol "://" host))))
