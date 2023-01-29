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
  (define-module (tegfs web-form-template-generic)
    :export (web::form-template/generic))))


(define (web::form-template/generic wide? form-params insides outsides)
  (lambda _
    (display "
<div class='centering-container'>
  <div class='capped-width'>
    <div class='tiled dark")
    (when wide? (display " wide"))
    (display " smooth-edged'> <form ")
    (when form-params (display form-params))
    (display " method='post'>")
    (cond
     ((string? insides) (display insides))
     ((procedure? insides) (insides)))
    (display "</form> </div>")
    (cond
     ((string? outsides) (display outsides))
     ((procedure? outsides) (outsides)))
    (display "</div> </div>")))
