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

%var web::make-generic-box-response

%use (stringf) "./euphrates/stringf.scm"
%use (web::form-template) "./web-form-template.scm"
%use (web::make-html-response) "./web-make-html-response.scm"

(define (web::make-generic-box-response status fmt . args)
  (define inner (apply stringf (cons fmt args)))
  (define html (web::form-template #f inner))
  (web::make-html-response html #:status status))
