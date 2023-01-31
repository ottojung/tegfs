;;;; Copyright (C) 2022  Otto Jung
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
  (define-module (tegfs web-set-cookie-header)
    :export (web::set-cookie-header)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates tilda-a) :select (~a))
    )))



(define (web::set-cookie-header key value share-time)
  (cons 'set-cookie
        (string-append (~a key) "=" (~a value)
                       " ; HttpOnly ; Secure ; SameSite=Lax ;"
                       (if share-time
                           (if (number? share-time)
                               (string-append "Max-Age=" (~a share-time) " ;")
                               (raisu 'share-time-should-be-a-number))
                           ""))))
