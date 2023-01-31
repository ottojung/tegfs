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
  (define-module (tegfs web-form-template-wide)
    :export (web::form-template/wide)
    :use-module ((tegfs web-form-template-generic) :select (web::form-template/generic))
    )))



(define web::form-template/wide
  (case-lambda
   ((form-params insides) (web::form-template/generic #t form-params insides #f))
   ((form-params insides outsides) (web::form-template/generic #t form-params insides outsides))))
