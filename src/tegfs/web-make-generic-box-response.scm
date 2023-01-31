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
  (define-module (tegfs web-make-generic-box-response)
    :export (web::make-generic-box-response)
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((tegfs web-form-template) :select (web::form-template))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    )))



(define (web::make-generic-box-response status fmt . args)
  (define inner (apply stringf (cons fmt args)))
  (define html (web::form-template #f inner))
  (web::make-html-response html #:status status))
