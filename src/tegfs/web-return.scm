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

(cond-expand
 (guile
  (define-module (tegfs web-return)
    :export (web::return)
    :use-module ((tegfs web-basic-headers) :select (web::basic-headers))
    )))



(cond-expand
 (guile
  (use-modules (web response))
  ))

(define (web::return status-code additional-headers maybe-body)
  (values
   (build-response
    #:code status-code
    #:headers
    (if additional-headers
        (append additional-headers web::basic-headers)
        web::basic-headers))
   maybe-body))
